-- | Parser for Prolog-style terms and equations.
--
-- * Variables start with an uppercase letter: @X@, @Foo@
-- * Functors are lowercase identifiers optionally followed by parenthesised arguments: @f(X, g(a))@
-- * Integers are atoms (they cannot take arguments): @42@, @-7@
-- * Equations are two terms separated by @=@: @f(X) = f(a)@
--
-- Whitespace between tokens is ignored, so @f (a)@ is the same as @f(a)@.
module Dovetail.Parser
  ( Parser
  , ParseError
  , parseTerm
  , parseEquation
  , term
  , equation
  ) where

import Text.Parsec

import Dovetail (Equation (..), Name, Term (..), atom)
import Dovetail.Parser.Lexer

type Parser = Parsec [Lexeme] ()

parseTerm :: String -> Either ParseError Term
parseTerm = runTokens term

parseEquation :: String -> Either ParseError Equation
parseEquation = runTokens equation

runTokens :: Parser a -> String -> Either ParseError a
runTokens p input = do
  toks <- tokenize "" input
  runParser (startAt toks *> p <* end) () "" toks
  where
    startAt (l : _) = setPosition (lexemeStart l)
    startAt [] = pure ()

equation :: Parser Equation
equation = Equation <$> term <* symbol TokEquals <*> term

term :: Parser Term
term = variable <|> functor <|> integer <?> "term"

variable :: Parser Term
variable = satisfyToken $ \case
  TokVar n -> Just (Var n)
  _ -> Nothing

integer :: Parser Term
integer = satisfyToken $ \case
  TokInt n -> Just (atom (show n))
  _ -> Nothing

functor :: Parser Term
functor = Fun <$> name <*> option [] args
  where
    name :: Parser Name
    name = satisfyToken $ \case
      TokName n -> Just n
      _ -> Nothing

    args :: Parser [Term]
    args = between (symbol TokLParen) (symbol TokRParen) (sepBy1 term (symbol TokComma))

symbol :: Token -> Parser ()
symbol t = satisfyToken (\t' -> if t == t' then Just () else Nothing) <?> showToken t

satisfyToken :: (Token -> Maybe a) -> Parser a
satisfyToken f = tokenPrim (showToken . lexemeToken) nextPos (f . lexemeToken)
  where
    nextPos _ _ (next : _) = lexemeStart next
    nextPos _ current [] = lexemeEnd current

end :: Parser ()
end =
  getInput >>= \case
    [] -> pure ()
    l : _ -> setPosition (lexemeStart l) *> unexpected (showToken (lexemeToken l)) <?> "end of input"
