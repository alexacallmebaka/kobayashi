--lex input file.
module Lexer (
    ) where

--imports {{{1
import KBYToken

import qualified Data.Text as T
import Data.Void

import Control.Applicative hiding (some, many)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
--1}}}

type Parser = Parsec Void T.Text

file :: Parser KBYStream --{{{1
file = KBYStream <$> (many $ choice
                            [ block
                            , inline
                            , endOfBlockOrSpace ]) <* eof
--1}}}

-- block stuff {{{1
block :: Parser KBYWithInfo
block = choice
    [ header <?> "header or subheader" ]

-- headers {{{2
headerPrefix :: Parser ()
headerPrefix = () <$ char '@'

header :: Parser KBYWithInfo
header = do
    startPos <- getSourcePos
    headerPrefix
    tok <- option BeginHeader (BeginSubheader <$ headerPrefix)
    let txt = T.pack $ case tok of
                           BeginHeader -> "@"
                           BeginSubheader -> "@@"
    return $ KBYWithInfo startPos txt tok
    
-- 2}}}

--1}}}

-- inline stuff {{{1
inline :: Parser KBYWithInfo
inline = choice
    [ basicInline '*' Bold <?> "bold"
    , basicInline '/' Italic <?> "italic"
    , textChar <?> "printable unicode char"]

basicInline :: Char -> KBYToken -> Parser KBYWithInfo
basicInline tokChar tok = do 
    startPos <- getSourcePos
    txt <- char tokChar
    return $ KBYWithInfo startPos (T.singleton txt) tok

textChar :: Parser KBYWithInfo
textChar = do 
    startPos <- getSourcePos
    escaped <- (option '\00' (char '\\'))
    trueChar <- printChar
    let txt = case escaped of
                '\00' -> T.singleton trueChar
                x -> T.pack $ x:trueChar:[]
    return $ KBYWithInfo startPos txt TextChar
-- 1}}}

endOfBlockOrSpace :: Parser KBYWithInfo
endOfBlockOrSpace = do 
    startPos <- getSourcePos
    eol
    tok <- option TextChar (EndOfBlock <$ some eol)
    let txt = T.pack $ case tok of
                           TextChar -> " "
                           EndOfBlock -> "\\n\\n"
    return $ KBYWithInfo startPos txt tok

tokenize :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) KBYStream 
tokenize = runParser file
