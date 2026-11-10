module Dovetail.Parser.Lexer
  ( Token (..)
  , Lexeme (..)
  , tokenize
  , showToken
  ) where

import Text.Parsec
import Text.Parsec.String (Parser)

import Dovetail (Name)

data Token
  = TokVar Name
  | TokName Name
  | TokInt Integer
  | TokLParen
  | TokRParen
  | TokComma
  | TokEquals
  deriving (Eq, Show)

data Lexeme = Lexeme
  { lexemeStart :: SourcePos
  , lexemeEnd :: SourcePos
  , lexemeToken :: Token
  }
  deriving (Eq, Show)

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
    [ TokVar <$> ident upper
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
