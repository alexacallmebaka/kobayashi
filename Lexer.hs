--lex input file.
module Lexer
    ( Token(..)
    , TokenPos
    , tokenize
    ) where

import Control.Applicative hiding ((<|>),many)
import Text.Parsec hiding (token, tokens)

data Token = Header | Subheader | Bold | Italic | EOL | Break | PlainText String deriving (Eq, Show)

type TokenPos = (Token, SourcePos)

--a file is several lines of tokens.

tokens = many token

--a token is either header or text related.
token = do
    p <- getPosition 
    t <- header <|> text <|> linebreakOrEol
    return (t,p)

--a single @ denotes a header, and two @ denote a subheader.
header = char '@' *> option Header (char '@' *> return Subheader) <?> "header or subheader"

--text {{{1

--text can be bold, italic or plain.
text = bold <|> italic <|> PlainText <$> many1 textChar <?> "text"

-- * is a bold token.
bold = string "*" *> return Bold <?> "bold"

-- / is an italic token.
italic = string "/" *> return Italic <?> "italic"

--a text char can either be an escaped char or just any valid char.
textChar = escapedChar <|> noneOf (metaChars ++ "\n\r") <?> "valid text character"

--an escaped char is prefixed with a \.
escapedChar = char '\\' *> noneOf "\n\r" <?> "escape"

--metachars are anything used in text streams to denote something else.
metaChars = "*\\/"

--1}}}

--just one newline is eol, two newlines in line break. We also count newline then eof as break.
linebreakOrEol = eol *> option EOL ((eol <|> eof) *> return Break) <?> "end of line or linebreak" 

--eol can be many things.
eol = choice [ try (string "\n\r")
    , try (string "\r\n")
    , string "\n"
    , string "\r"
    ] *> return () <?> "end of line"

tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser tokens ()
