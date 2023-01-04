module Lexer
  ( Token(..)
  , tokenize
  ) where

import Control.Applicative hiding ((<|>),many)
import Text.Parsec hiding (token, tokens)

data Token = Header | Subheader | Bold | Italic | PlainText String deriving (Eq, Show)

tokens = many token

token = try subheader <|> header <|> text

subheader = string "## " *> return Subheader

header = string "# " *> return Header

--Text {{{1
text = bold <|> italic <|> PlainText <$> many1 textChar

bold = string "*" *> return Bold

italic = string "/" *> return Italic

textChar = escapedChar <|> noneOf (metaChars ++ "\n\r")

escapedChar = char '\\' *> oneOf metaChars

metaChars = "*\\/"
--1}}}

--eol = choice [ try (string "\n\r")
--             , try (string "\r\n")
--             , string "\n"
--             , string "\r"
--             ]

tokenize :: SourceName -> String -> Either ParseError [Token]
tokenize = runParser tokens ()
