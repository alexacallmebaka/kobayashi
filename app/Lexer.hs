{-# LANGUAGE OverloadedStrings #-}

--lex input file.
module Lexer (
    tokenize
    ) where

--imports {{{1
import KBYToken

import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Void

import Control.Applicative hiding (some, many)

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
--1}}}

type Parser = Parsec Void T.Text

file :: Parser KBYStream
file = KBYStream . concat <$> (some (choice [psuedoBlock, block]) <* eof)

-- block stuff {{{1
-- right now our only true block is a paragraph.
block :: Parser [KBYWithInfo]
block = do 
    (contents, eob) <- someTill_ inline (try endOfBlock)
    return $ (concat contents) ++ [eob]

--elements that act as block elements but functionally take up one line.
psuedoBlock :: Parser [KBYWithInfo]
psuedoBlock = choice [ psuedoBlockWithPrefix 
                     , image ]

psuedoBlockWithPrefix :: Parser [KBYWithInfo]
psuedoBlockWithPrefix = do
    prefix <- psuedoBlockPrefix
    (contents, eob) <- someTill_ inline (try endOfBlock)
    return $ [prefix] ++ (concat contents) ++ [eob]

psuedoBlockPrefix :: Parser KBYWithInfo
psuedoBlockPrefix = choice
    [ headerPrefix <?> "header or subheader"]

--headers {{{2

headerPrefix :: Parser KBYWithInfo
headerPrefix = do
    startPos <- getSourcePos
    char '@'
    tok <- option BeginHeader (BeginSubheader <$ char '@')
    let txt = case tok of
               BeginHeader -> "@"
               BeginSubheader -> "@@"
    return $ KBYWithInfo startPos txt tok
    
--2}}}

image :: Parser [KBYWithInfo]
image = do
  beginPos <- getSourcePos
  start <- basicInline '<' BeginImg
  (content,end) <- manyTill_ textChar ( try (basicInline '>' EndImg) )
  eob <- endOfBlock
  return $ [start] ++ content ++ [end] ++ [eob]

--1}}}

-- inline stuff {{{1

--either a link or a single rich text car, we use singleton lists so types match up.
--links are lexed separately to avoid reading in / as italic in the link source.
inline :: Parser [KBYWithInfo]
inline = try link <|> (richTextChar >>= (\x -> return $ x:[]))

richTextChar :: Parser KBYWithInfo
richTextChar = choice
    [ basicInline '*' Bold <?> "bold"
    , basicInline '/' Italic <?> "italic"
    , textChar <?> "printable unicode char"]

basicInline :: Char -> KBYToken -> Parser KBYWithInfo
basicInline tokChar tok = do 
    startPos <- getSourcePos
    txt <- char tokChar
    return $ KBYWithInfo startPos (T.singleton txt) tok

link :: Parser [KBYWithInfo]
link = do
    start <- basicInline '[' LinkStart
    --fail if early link end char.
    (title, linkSep) <- manyTill_ ((char ']' >> (failure Nothing Set.empty)) <|> richTextChar) (basicInline '|' LinkSep)
    (href, end) <- manyTill_ textChar (basicInline ']' LinkEnd)
    return $ [start] ++ title ++ [linkSep] ++ href ++ [end]

textChar :: Parser KBYWithInfo
textChar = do 
    startPos <- getSourcePos
    option '\00' (char '\\')
    txt <- printChar <|> newline
    return $ KBYWithInfo startPos (T.singleton txt) TextChar
--1}}}

endOfBlock :: Parser KBYWithInfo
endOfBlock = do
    startPos <- getSourcePos
    eol
    (() <$ eol) <|> eof
    let txt = "\\n\\n"
    return $ KBYWithInfo startPos txt EndOfBlock


tokenize :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) KBYStream 
tokenize = runParser file
