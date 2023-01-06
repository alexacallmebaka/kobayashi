module Lexer
  ( Token(..)
  , tokenize
  ) where

import Control.Applicative hiding ((<|>),many)
import Text.Parsec hiding (token, tokens)

data Token = Header | Subheader | Bold | Italic | PlainText String deriving (Eq, Show)

file = endBy tokens eol

tokens = many token

token = header <|> text

header = char '@' *> option Header (char '@' *> return Subheader) <?> "header or subheader"

--Text {{{1
text = bold <|> italic <|> PlainText <$> many1 textChar <?> "text"

bold = string "*" *> return Bold <?> "bold text '*'"

italic = string "/" *> return Italic <?> "italic text '/'"

textChar = escapedChar <|> noneOf (metaChars ++ "\n\r") <?> "valid text character"

escapedChar = char '\\' *> noneOf "\n\r" <?> "an escaped metacharacter, escape with \\"

metaChars = "*\\/"
--1}}}

eol = choice [ try (string "\n\r")
             , try (string "\r\n")
             , string "\n"
             , string "\r"
             ] <?> "end of line"

tokenize :: SourceName -> String -> Either ParseError [Token]
tokenize = runParser tokens ()
