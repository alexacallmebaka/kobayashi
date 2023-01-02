module Lexer
  ( Token(..)
  , tokenize
  ) where

import Control.Applicative hiding ((<|>),many)
import Text.Parsec hiding (token, tokens)

data Token = Header | Subheader | Bold | Italic | Text Char deriving Show

tokens = many token

token = try subheader <|> header <|> text

subheader = string "## " *> return Subheader

header = string "# " *> return Header

--Text {{{1
text = try bold <|> italic <|> Text <$> textChar

bold = string "**" *> return Bold

italic = string "*" *> return Italic

textChar = escapedChar <|> noneOf (metaChars ++ "\n\r")

escapedChar = char '\\' *> oneOf metaChars

metaChars = "*\\"
--1}}}

tokenize :: SourceName -> String -> Either ParseError [Token]
tokenize = runParser tokens ()
