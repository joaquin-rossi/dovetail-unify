-- | Tokenizer for "Dovetail.Parser". Whitespace between tokens is discarded.
module Dovetail.Parser.Lexer
  ( Token (..)
  , Lexeme (..)
  , tokenize
  , showToken
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import Dovetail (Name)

-- | A token of the term syntax.
data Token
  = TokVar Name
  | TokName Name
  | TokInt Integer
  | TokLParen
  | TokRParen
  | TokComma
  | TokEquals
  deriving (Eq, Show)

-- | A token together with where it starts and ends in the input.
data Lexeme = Lexeme
  { lexemeStart :: SourcePos
  , lexemeEnd :: SourcePos
  , lexemeToken :: Token
  }
  deriving (Eq, Show)

-- | Split the input into tokens. The source name is only used in errors.
tokenize :: SourceName -> String -> Either ParseError [Lexeme]
tokenize = parse (spaces *> many (lexeme <* spaces) <* eof)

lexeme :: Parser Lexeme
lexeme = do
  start <- getPosition
  t <- tok
  Lexeme start <$> getPosition <*> pure t

tok :: Parser Token
tok =
  choice
    [ TokVar <$> ident (upper <|> char '_')
    , TokName <$> ident lower
    , TokInt <$> integer
    , TokLParen <$ char '('
    , TokRParen <$ char ')'
    , TokComma <$ char ','
    , TokEquals <$ char '='
    ]
    <?> "token"
  where
    ident start = (:) <$> start <*> many identChar
    identChar = alphaNum <|> char '_'
    integer = option id (negate <$ char '-') <*> (read <$> many1 digit) <* notFollowedBy identChar

-- | Render a token as it appears in the input, quoted, for error messages.
showToken :: Token -> String
showToken = show . text
  where
    text (TokVar n) = n
    text (TokName n) = n
    text (TokInt n) = show n
    text TokLParen = "("
    text TokRParen = ")"
    text TokComma = ","
    text TokEquals = "="
