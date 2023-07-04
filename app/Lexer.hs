{-# LANGUAGE OverloadedStrings #-}

--lex input file.
module Lexer (
    ) where

import qualified Token as KBYToken

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

header :: Parser KBYToken.Token
header = headerPrefix *> option KBYToken.BeginHeader (KBYToken.BeginSubheader <$ headerPrefix)
-- 1}}}

line :: Parser [KBYToken.Token]
line = many $ choice
    [ bold
    , italic
    , textChar ]

-- inline stuff {{{1
bold :: Parser KBYToken.Token
bold = KBYToken.Bold <$ char '*' <?> "bold"

italic :: Parser KBYToken.Token
italic = KBYToken.Italic <$ char '/' <?> "italic"

textChar :: Parser KBYToken.Token
textChar = KBYToken.TextChar . T.singleton <$> (optional (char '\\') *> printChar)
-- 1}}}
