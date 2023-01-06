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

header = char '@' *> option Header (char '@' *> return Subheader)

--Text {{{1
text = bold <|> italic <|> PlainText <$> many1 textChar

bold = string "*" *> return Bold

italic = string "/" *> return Italic

textChar = escapedChar <|> noneOf (metaChars ++ "\n\r")

escapedChar = char '\\' *> oneOf metaChars

metaChars = "*\\/"
--1}}}

eol = choice [ try (string "\n\r")
             , try (string "\r\n")
             , string "\n"
             , string "\r"
             ]

tokenize :: SourceName -> String -> Either ParseError [Token]
tokenize = runParser tokens ()
