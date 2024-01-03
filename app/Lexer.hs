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
-- <* eof to ensure we fail if we havent reached the end of the file yet.
-- you cant consume eof, so as long as we are at the end this parser will succeed.
file = KBYStream . concat <$> (some (choice [psuedoBlock, block]) <* eof)

-- block stuff {{{1

block :: Parser [KBYWithInfo]
block = choice [ unorderedList
               , codeListing
               , paragraph ]

paragraph :: Parser [KBYWithInfo]
paragraph = do 
    (contents, eob) <- someTill_ inline (try endOfBlock)
    return $ (concat contents) ++ [eob]

unorderedList :: Parser [KBYWithInfo]
unorderedList = do
    (contents, eob) <- someTill_ unorderedListItem (try unorderedListEnd)
    return $ (concat contents) ++ [eob]

unorderedListItem :: Parser [KBYWithInfo]
unorderedListItem =  do
  bullet <- basicInline '-' UnorderedListItem
  space
  contents <- someTill inline (try eol)
  return $ [bullet] ++ (concat contents)

--cannot use the standard endOfBlock parser here since the unorderedListItem "eats" the first newline.
unorderedListEnd :: Parser KBYWithInfo
unorderedListEnd = do
    startPos <- getSourcePos
    (() <$ eol) <|> eof
    let txt = "\\n\\n"
    return $ KBYWithInfo startPos txt EndOfBlock

codeListing :: Parser [KBYWithInfo]
codeListing = do
  start <- listingMarker BeginCodeListing
  eol
  (contents,end) <- someTill_ textChar (listingMarker EndCodeListing)
  eob <- endOfBlock
  return $ [start] ++ contents ++ [end] ++ [eob]

  
listingMarker :: KBYToken -> Parser KBYWithInfo
listingMarker beginOrEnd = do
    startPos <- getSourcePos
    string "```"
    return $ KBYWithInfo startPos "```" beginOrEnd


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
  start <- basicInline '<' BeginImg
  maybeAssetRef <- optional $ basicInline '$' AssetRef
  (content,end) <- manyTill_ textChar ( try (basicInline '>' EndImg) )
  eob <- endOfBlock
  case maybeAssetRef of
    (Just ref) -> return $ [start] ++ [ref]  ++ content ++ [end] ++ [eob]
    Nothing -> return $ [start] ++ content ++ [end] ++ [eob]

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
    , basicInline '`' Verb <?> "inline verbatim"
    , textChar <?> "printable unicode char"]

basicInline :: Char -> KBYToken -> Parser KBYWithInfo
basicInline tokChar tok = do 
    startPos <- getSourcePos
    txt <- char tokChar
    return $ KBYWithInfo startPos (T.singleton txt) tok

pageOrAssetRef :: Parser KBYWithInfo
pageOrAssetRef = basicInline '%' PageRef <|> basicInline '$' AssetRef

link :: Parser [KBYWithInfo]
link = do
    start <- basicInline '[' LinkStart
    --fail if early link end char.
    (title, linkSep) <- manyTill_ ((char ']' >> (failure Nothing Set.empty)) <|> richTextChar) (basicInline '|' LinkSep)
    maybeRefType <- optional pageOrAssetRef
    (href, end) <- manyTill_ textChar (basicInline ']' LinkEnd)
    case maybeRefType of
      (Just refType) -> return $ [start] ++ title ++ [linkSep] ++ [refType] ++ href ++ [end]
      Nothing -> return $ [start] ++ title ++ [linkSep] ++ href ++ [end]

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
