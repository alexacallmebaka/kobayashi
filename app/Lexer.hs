--lex input file.
module Lexer (
    lexFile
    ) where

--imports {{{1
import Token
import KBYDoc (Document)

import qualified Data.Text as T
import qualified Data.Set as Set
import Data.Void

import Control.Applicative hiding (some, many)

import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

import Error (BuildError(..))
--1}}}

type Parser = Parsec Void T.Text

file :: Parser TokenStream
-- <* eof to ensure we fail if we havent reached the end of the file yet.
-- you cant consume eof, so as long as we are at the end this parser will succeed.
file = TokenStream . concat <$> (some (choice [psuedoBlock, block]) <* eof)

-- block stuff {{{1

block :: Parser [RichToken]
block = choice [ unorderedList
               , codeListing
               , paragraph ]

paragraph :: Parser [RichToken]
paragraph = do 
    (contents, eob) <- someTill_ inline (try endOfBlock)
    return $ (concat contents) ++ [eob]

unorderedList :: Parser [RichToken]
unorderedList = do
    (contents, eob) <- someTill_ unorderedListItem (try unorderedListEnd)
    return $ (concat contents) ++ [eob]

unorderedListItem :: Parser [RichToken]
unorderedListItem =  do
  bullet <- basicInline '-' UnorderedListItem
  space
  contents <- someTill inline (try eol)
  return $ [bullet] ++ (concat contents)

--cannot use the standard endOfBlock parser here since the unorderedListItem "eats" the first newline.
unorderedListEnd :: Parser RichToken
unorderedListEnd = do
    startPos <- getSourcePos
    (() <$ eol) <|> eof
    let txt = "\\n\\n"
    return $ RichToken startPos txt EndOfBlock

codeListing :: Parser [RichToken]
codeListing = do
  start <- listingMarker BeginCodeListing
  eol
  (contents,end) <- someTill_ textChar (listingMarker EndCodeListing)
  eob <- endOfBlock
  return $ [start] ++ contents ++ [end] ++ [eob]

  
listingMarker :: Token -> Parser RichToken
listingMarker beginOrEnd = do
    startPos <- getSourcePos
    string "```"
    return $ RichToken startPos "```" beginOrEnd


--elements that act as block elements but functionally take up one line.
psuedoBlock :: Parser [RichToken]
psuedoBlock = choice [ psuedoBlockWithPrefix 
                     , image ]

psuedoBlockWithPrefix :: Parser [RichToken]
psuedoBlockWithPrefix = do
    prefix <- psuedoBlockPrefix
    (contents, eob) <- someTill_ inline (try endOfBlock)
    return $ [prefix] ++ (concat contents) ++ [eob]

psuedoBlockPrefix :: Parser RichToken
psuedoBlockPrefix = choice
    [ headerPrefix <?> "header or subheader"]

--headers {{{2

headerPrefix :: Parser RichToken
headerPrefix = do
    startPos <- getSourcePos
    char '@'
    tok <- option BeginHeader (BeginSubheader <$ char '@')
    let txt = case tok of
               BeginHeader -> "@"
               BeginSubheader -> "@@"
    return $ RichToken startPos txt tok
    
--2}}}

image :: Parser [RichToken]
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
inline :: Parser [RichToken]
inline = try link <|> (richTextChar >>= (\x -> return $ x:[]))

richTextChar :: Parser RichToken
richTextChar = choice
    [ basicInline '*' Bold <?> "bold"
    , basicInline '/' Italic <?> "italic"
    , basicInline '`' Verb <?> "inline verbatim"
    , textChar <?> "printable unicode char"]

basicInline :: Char -> Token -> Parser RichToken
basicInline tokChar tok = do 
    startPos <- getSourcePos
    txt <- char tokChar
    return $ RichToken startPos (T.singleton txt) tok

pageOrAssetRef :: Parser RichToken
pageOrAssetRef = basicInline '%' PageRef <|> basicInline '$' AssetRef

link :: Parser [RichToken]
link = do
    start <- basicInline '[' LinkStart
    --fail if early link end char.
    (title, linkSep) <- manyTill_ ((char ']' >> (failure Nothing Set.empty)) <|> richTextChar) (basicInline '|' LinkSep)
    maybeRefType <- optional pageOrAssetRef
    (href, end) <- manyTill_ textChar (basicInline ']' LinkEnd)
    case maybeRefType of
      (Just refType) -> return $ [start] ++ title ++ [linkSep] ++ [refType] ++ href ++ [end]
      Nothing -> return $ [start] ++ title ++ [linkSep] ++ href ++ [end]

textChar :: Parser RichToken
textChar = do 
    startPos <- getSourcePos
    option '\00' (char '\\')
    txt <- printChar <|> newline
    return $ RichToken startPos (T.singleton txt) TextChar
--1}}}

endOfBlock :: Parser RichToken
endOfBlock = do
    startPos <- getSourcePos
    eol
    (() <$ eol) <|> eof
    let txt = "\\n\\n"
    return $ RichToken startPos txt EndOfBlock


tokenize :: String -> T.Text -> Either (ParseErrorBundle T.Text Void) TokenStream 
tokenize = runParser file

lexFile :: String -> T.Text -> Either BuildError TokenStream --{{{2
lexFile source input = case tokenize source input of
        Left err -> Left . LexError . T.pack $ errorBundlePretty err
        Right tokens -> Right tokens
--2}}}
