{-# LANGUAGE OverloadedStrings #-}

--lex input file.
module Lexer (
    ) where

import qualified Token as KBYToken

import qualified Data.Text as T
import Data.Void
import Control.Applicative hiding (some, many)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void T.Text

line :: Parser [KBYToken.Token]
line = many $ choice
    [ bold
    , italic
    , textChar ]

-- inline stuff {{{1
bold :: Parser KBYToken.Token
bold = KBYToken.Bold <$ char '*'

italic :: Parser KBYToken.Token
italic = KBYToken.Italic <$ char '/'

textChar :: Parser KBYToken.Token
textChar = KBYToken.TextChar . T.singleton <$> (optional (char '\\') *> printChar)
-- 1}}}
