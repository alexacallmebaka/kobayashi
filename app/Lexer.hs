{-# LANGUAGE OverloadedStrings #-}

-- todo errors, lexFile function

--lex input file.
module Lexer (
    ) where

--imports {{{1
import KBYToken

import qualified Data.Text as T
import Data.Void

import Control.Applicative hiding (some, many)

import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char
--1}}}

type Parser = Parsec Void T.Text

file :: Parser [KBYToken]
file = many $ choice
                [ block
                , inline
                , endOfBlockOrSpace ]

-- block stuff {{{1
block :: Parser KBYToken
block = choice
    [ header ]
-- headers {{{2
headerPrefix :: Parser ()
headerPrefix = () <$ char '@' 

header :: Parser KBYToken
header = headerPrefix *> option BeginHeader (BeginSubheader <$ headerPrefix)
-- 2}}}
--1}}}

-- inline stuff {{{1
inline :: Parser KBYToken
inline = choice
    [ bold
    , italic
    , textChar ]

bold :: Parser KBYToken
bold = Bold <$ char '*' <?> "bold"

italic :: Parser KBYToken
italic = Italic <$ char '/' <?> "italic"

textChar :: Parser KBYToken
textChar = TextChar . T.singleton <$> (optional (char '\\') *> printChar)
-- 1}}}

endOfBlockOrSpace :: Parser KBYToken
endOfBlockOrSpace = eol *> option (TextChar " ") (EndOfBlock <$ eol)
