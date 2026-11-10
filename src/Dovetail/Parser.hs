-- | Parser for Prolog-style terms and equations.
--
-- * Variables start with an uppercase letter or an underscore: @X@, @Foo@, @_Bar@
-- * Each anonymous variable @_@ is a distinct variable, named @_1@, @_2@, ...
-- * Functors are lowercase identifiers optionally followed by parenthesised arguments: @f(X, g(a))@
-- * Integers are atoms (they cannot take arguments): @42@, @-7@
-- * Equations are two terms separated by @=@: @f(X) = f(a)@
-- * Systems of equations are separated by commas: @X = f(Y), Y = a@
--
-- Whitespace between tokens is ignored, so @f (a)@ is the same as @f(a)@.
module Dovetail.Parser
  ( Parser
  , ParseError
  , parseTerm
  , parseEquation
  , parseEquations
  , term
  , equation
  , equations
  ) where

import Data.List (mapAccumL)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Parsec

import Dovetail (Equation (..), Name, Term (..), atom, vars)
import Dovetail.Parser.Lexer

-- | A parser over the tokens produced by 'tokenize'.
type Parser = Parsec [Lexeme] ()

-- | Parse a single term.
parseTerm :: String -> Either ParseError Term
parseTerm input = do
  t <- runTokens term input
  pure (snd (nameAnonymous (vars t) 1 t))

-- | Parse a single equation.
parseEquation :: String -> Either ParseError Equation
parseEquation input = do
  e <- runTokens equation input
  pure (snd (nameAnonymousEq (equationVars e) 1 e))

-- | Parse one or more comma-separated equations.
parseEquations :: String -> Either ParseError [Equation]
parseEquations input = do
  es <- runTokens equations input
  pure (snd (mapAccumL (nameAnonymousEq (foldMap equationVars es)) 1 es))

runTokens :: Parser a -> String -> Either ParseError a
runTokens p input = do
  toks <- tokenize "" input
  runParser (startAt toks *> p <* end) () "" toks
  where
    startAt (l : _) = setPosition (lexemeStart l)
    startAt [] = pure ()

-- | One or more comma-separated equations.
equations :: Parser [Equation]
equations = sepBy1 equation (symbol TokComma)

-- | Two terms separated by @=@.
equation :: Parser Equation
equation = Equation <$> term <* symbol TokEquals <*> term

-- | A variable, a functor with optional arguments, or an integer atom.
--
-- Anonymous variables are returned as @Var "_"@; the @parse*@ functions
-- give each of them a distinct name.
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

-- | Replace each anonymous variable with the next @_N@ not in the given set.
nameAnonymous :: Set Name -> Int -> Term -> (Int, Term)
nameAnonymous used = go
  where
    go i (Var "_") =
      let j = until ((`Set.notMember` used) . name) (+ 1) i
       in (j + 1, Var (name j))
    go i t@(Var _) = (i, t)
    go i (Fun f ts) = Fun f <$> mapAccumL go i ts

    name j = '_' : show j

nameAnonymousEq :: Set Name -> Int -> Equation -> (Int, Equation)
nameAnonymousEq used i (Equation l r) =
  let (i', l') = nameAnonymous used i l
      (i'', r') = nameAnonymous used i' r
   in (i'', Equation l' r')

equationVars :: Equation -> Set Name
equationVars (Equation l r) = vars l <> vars r

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
