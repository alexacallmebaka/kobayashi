{-# LANGUAGE OverloadedStrings #-}

--lex input file.
module Lexer (
    ) where

import KBYToken

import qualified Data.Text as T
import Data.Void

import Control.Applicative hiding (some, many)

--import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text

--spaces :: Parser ()
--spaces = L.space eol empty empty

-- headers {{{1
headerPrefix :: Parser ()
headerPrefix = () <$ char '@' 

header :: Parser KBYToken
header = headerPrefix *> option BeginHeader (BeginSubheader <$ headerPrefix)
-- 1}}}

line :: Parser [KBYToken]
line = many $ choice
    [ bold
    , italic
    , textChar ]

-- inline stuff {{{1
bold :: Parser KBYToken
bold = Bold <$ char '*' <?> "bold"

italic :: Parser KBYToken
italic = Italic <$ char '/' <?> "italic"

textChar :: Parser KBYToken
textChar = TextChar . T.singleton <$> (optional (char '\\') *> printChar)
-- 1}}}
