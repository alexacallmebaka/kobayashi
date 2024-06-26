--lex text into a stream of tokens.

--exports {{{1
module Lexer 
  (
    basicInline,
    inline,
    Parser,
    plainChar,
    tokenize,
  ) where
--1}}}

--imports {{{1

import Control.Applicative (optional, (<|>))
import Control.Monad.Combinators (choice, many, manyTill_, option, some, someTill_, someTill)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec (eof, errorBundlePretty, failure, getSourcePos, try, Parsec, runParser, (<?>))
import Text.Megaparsec.Char (alphaNumChar, char, eol, newline, printChar, string, space)
import Token (Token(..), TokenStream(..), RichToken(..))

import qualified Data.Text as Text
import qualified Data.Set as Set

import Document (Document)
import Error (BuildError(..))

--1}}}


--type alias to make signitures nicer
type Parser = Parsec Void Text

{- eof to ensure we fail if we havent reached the end of the file yet.
you cant consume eof, so as long as we are at the end this parser will succeed.
-}
file :: Parser TokenStream
file = do 
         title <- titleMarker
         pvImg <- option [] (try previewImage)
         pvDesc <- option [] (try previewDesc) 
         contents <- many (space *> choice [psuedoBlock, block])
         eof
         pure . TokenStream $ title ++ pvImg ++ pvDesc ++ (concat contents)

-- block stuff {{{1

block :: Parser [RichToken]
block = choice [ unorderedList
               , codeListing
               , blockQuote
               , paragraph ]

paragraph :: Parser [RichToken]
paragraph = do 
    (contents, eob) <- someTill_ inline (try endOfBlock)
    pure $ (concat contents) ++ [eob]

unorderedList :: Parser [RichToken]
unorderedList = do
    (contents, eob) <- someTill_ unorderedListItem (try unorderedListEnd)
    pure $ (concat contents) ++ [eob]

unorderedListItem :: Parser [RichToken]
unorderedListItem =  do
  space
  bullet <- basicInline '-' UnorderedListItem
  space
  contents <- someTill inline (try eol)
  pure $ [bullet] ++ (concat contents)

--cannot use the standard endOfBlock parser here since the unorderedListItem "eats" the first newline.
unorderedListEnd :: Parser RichToken
unorderedListEnd = do
    startPos <- getSourcePos
    (() <$ eol) <|> eof
    let txt = "\\n\\n"
    pure $ RichToken startPos txt EndOfBlock

blockQuote :: Parser [RichToken]
blockQuote = do
    start <- basicInline '>' BlockQuote 
    space
    (contents, authMark) <- someTill_ inline (try $ basicInline '~' BlockQuoteAuthor)
    space
    (author,eob) <- someTill_ inline (try endOfBlock)
    pure $ [start] ++ (concat contents) ++ [authMark] ++ (concat author) ++ [eob]

codeListing :: Parser [RichToken]
codeListing = do
  start <- listingMarker BeginCodeListing
  eol
  (contents,end) <- someTill_ ( codeChar <|> richNewline) (listingMarker EndCodeListing)
  eob <- endOfBlock
  pure $ [start] ++ contents ++ [end] ++ [eob]

  
listingMarker :: Token -> Parser RichToken
listingMarker beginOrEnd = do
    startPos <- getSourcePos
    string "```"
    pure $ RichToken startPos "```" beginOrEnd


--elements that act as block elements but functionally take up one line.
psuedoBlock :: Parser [RichToken]
psuedoBlock = choice [ secMarker
                     , groupMarker
                     , image ]

titleMarker :: Parser [RichToken]
titleMarker = psuedoBlockWithMarker '#' BeginTitle

secMarker :: Parser [RichToken]
secMarker = psuedoBlockWithMarker '@' BeginSection
 
previewDesc :: Parser [RichToken]
previewDesc = do
  startPos <- getSourcePos
  char '#'
  space
  string "DESC:"
  space
  let beginPvDescTok = RichToken startPos "# DESC:"  BeginPvDesc
  (contents, eob) <- manyTill_ plainChar endOfBlock
  pure $ [beginPvDescTok] ++ contents ++ [eob]


previewImage :: Parser [RichToken]
previewImage = do 
  startPos <- getSourcePos
  char '#'
  space
  string "IMAGE:"
  space
  let beginPvImUrlTok = RichToken startPos "# IMAGE:"  BeginPvImUrl
  maybeAssetRef <- optional $ basicInline '$' AssetRef
  (contents, eob) <- manyTill_ plainChar singleEndOfBlock
  case maybeAssetRef of
    (Just ref) -> pure $ [beginPvImUrlTok] ++ [ref] ++ contents ++ [eob]
    Nothing -> pure $ [beginPvImUrlTok] ++ contents ++ [eob]             

--elements of the form *contents where * is some marker.
psuedoBlockWithMarker :: Char -> Token -> Parser [RichToken]
psuedoBlockWithMarker mark tok = do
  space
  start <- basicInline mark tok
  space
  (contents, eob) <- manyTill_ inline singleEndOfBlock
  pure $ [start] ++ (concat contents) ++ [eob]

image :: Parser [RichToken]
image = do
  start <- basicInline '<' BeginImg
  maybeAssetRef <- optional $ basicInline '$' AssetRef
  (content,end) <- manyTill_ plainChar ( try (basicInline '>' EndImg) )
  eob <- endOfBlock
  case maybeAssetRef of
    (Just ref) -> pure $ [start] ++ [ref]  ++ content ++ [end] ++ [eob]
    Nothing -> pure $ [start] ++ content ++ [end] ++ [eob]

groupMarker :: Parser [RichToken]
groupMarker = do
  start <- basicInline '{' BeginGroupLabel
  (content, end) <- someTill_ groupMarkerChar (try (basicInline '}' EndGroupLabel))
  eol 
  pure $ [start] ++ content ++ [end]

--1}}}

-- inline stuff {{{1

--either a link or a single rich text car, we use singleton lists so types match up.
--links are lexed separately to avoid reading in / as italic in the link source.
inline :: Parser [RichToken]
inline = try link <|> try verb <|> (richTextChar >>= (\x -> pure $ x:[]))

richTextChar :: Parser RichToken
richTextChar = choice
    [ basicInline '*' Bold <?> "bold"
    , basicInline '/' Italic <?> "italic"
    , textChar <?> "printable unicode char"]

basicInline :: Char -> Token -> Parser RichToken
basicInline tokChar tok = do 
    startPos <- getSourcePos
    txt <- char tokChar
    pure $ RichToken startPos (Text.singleton txt) tok

verb :: Parser [RichToken]
verb = do
    start <- basicInline '`' Verb 
    (text, end) <- manyTill_ codeChar (basicInline '`' Verb)
    pure $ [start] ++ text ++ [end]

link :: Parser [RichToken]
link = do
    start <- basicInline '[' LinkStart
    --fail if early link end char, chars need to be in singleton lists for types to check.
    (title, linkSep) <- manyTill_ ((char ']' >> (failure Nothing Set.empty)) <|> try verb <|> (richTextChar >>= \x -> pure [x]) ) (basicInline '|' LinkSep)
    maybeRefType <- optional (basicInline '$' AssetRef)
    (href, end) <- manyTill_ plainChar (basicInline ']' LinkEnd)
    case maybeRefType of
      (Just refType) -> pure $ [start] ++ (concat title) ++ [linkSep] ++ [refType] ++ href ++ [end]
      Nothing -> pure $ [start] ++ (concat title) ++ [linkSep] ++ href ++ [end]

--char for code listings and verb.
codeChar :: Parser RichToken
codeChar = (try $ escapedChar '\\') <|> (try $ escapedChar '`') <|> plainChar

--valid char for group marker.
groupMarkerChar :: Parser RichToken
groupMarkerChar = do
    startPos <- getSourcePos
    txt <- alphaNumChar <|> (char '-') <|> (char '_')
    pure $ RichToken startPos (Text.singleton txt) TextChar

--potentially escaped plaintext.
textChar :: Parser RichToken
textChar = do 
    startPos <- getSourcePos
    option '\00' (char '\\')
    --TODO: find a way to make this dos friendly.
    txt <- printChar <|> newline
    pure $ RichToken startPos (Text.singleton txt) TextChar

--literal plaintext, printable unicode.
plainChar :: Parser RichToken
plainChar = do
    startPos <- getSourcePos
    txt <- printChar
    pure $ RichToken startPos (Text.singleton txt) TextChar

richNewline = do
  startPos <- getSourcePos
  txt <- newline
  pure $ RichToken startPos (Text.singleton txt) TextChar

--there are a couple times we want a single newline
--to be treated as an end of block (title and section).
singleEndOfBlock = do
  startPos <- getSourcePos
  txt <- newline
  pure $ RichToken startPos "\\n" EndOfBlock

--parse a specific escaped char 
escapedChar :: Char -> Parser RichToken
escapedChar c = do
  startPos <- getSourcePos
  char '\\'
  lexeme <- char c
  pure $ RichToken startPos (Text.singleton lexeme) TextChar
   

endOfBlock :: Parser RichToken
endOfBlock = do
    startPos <- getSourcePos
    eol
    (() <$ eol) <|> eof
    let txt = "\\n\\n"
    pure $ RichToken startPos txt EndOfBlock
--1}}}

--run lexer.
tokenize :: String -> Text -> Either BuildError TokenStream --{{{2
tokenize source input = case runParser file source input of
        Left err -> Left . LexError . pack $ errorBundlePretty err
        Right tokens -> Right tokens
--2}}}
