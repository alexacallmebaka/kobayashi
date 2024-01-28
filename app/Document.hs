--kobayashi's intermediate representation.

--prgamas {{{1

--used to partially apply the MonadReader constraint to only types with "Options" as the environment type.
{-# LANGUAGE FlexibleContexts #-}

--1}}}

--exports {{{1
module Document
    (
      BlockElem(..)
    , Document(..)
    , Section(..)
    , InlineElem(..)
    , UnorderedListItem (..)
    , Url (..)
    ) where
--1}}}

--imports {{{1
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, ask)
import Data.Maybe (fromMaybe)
import Data.Text (append, pack, Text, unpack)
import Path (toFilePath)

import Html (htmlFold, Html, htmlify, includeCss, meta, motd, Tag(..), wrap)
import Options (Options(..))

import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified System.FilePath as SysPath
--1}}}


--types {{{1
data Document = Document 
     { docTitle :: [InlineElem]
     , docContents :: [Section] 
     }

data Section = Section
     { secTitle :: Maybe [InlineElem]
     , secContents :: [BlockElem]
     }

data Url = RemoteRef { refSrc :: Text }
         | PageRef { refSrc :: Text }
         | AssetRef { refSrc :: Text }
         deriving (Eq, Show, Ord)

data UnorderedListItem = UnorderedListItem [InlineElem] deriving (Eq, Show, Ord)

data BlockElem = Paragraph [InlineElem]
               | UnorderedList [UnorderedListItem]
               --1st list is quote, 2nd is author.
               | BlockQuote [InlineElem] [InlineElem]
               | Image Url
               --label + list of elements.
               | Group Text [BlockElem]
               --Text is the verbatim code.
               | CodeListing Text
               deriving (Eq, Show, Ord)

--inline elements that represent rich text can be arbitrarily nested.
data InlineElem = Bold [InlineElem]
                | Italic [InlineElem]
                | Link [InlineElem] Url
                | Verb Text
                | PlainText Text 
                deriving (Eq, Show, Ord)
--1}}}

--utility functions for turning ir => html. {{{1

--use html entities to make text safe. quite inefficient asymptotically but should be okay for now. {{{2
makeSafe :: Text -> Text
makeSafe = Text.replace "<" "&lt;" 
         . Text.replace ">" "&gt;"
         . Text.replace "\"" "&quot;"
         . Text.replace "'" "&apos;"
         . Text.replace "&" "&amp;"
--2}}}

--Get only textual components of Inline elements {{{2
extractRawText :: InlineElem -> Text
extractRawText (Verb txt) = txt
extractRawText (PlainText txt) = txt
extractRawText (Link title _ ) = Text.concat . map extractRawText $ title
extractRawText ( Bold txt ) = Text.concat . map extractRawText $ txt
extractRawText ( Italic txt ) = Text.concat . map extractRawText $ txt
--2}}}

--wrap HTML-encoded text in a section with a given title {{{2
wrapInSec :: ( MonadReader Options r ) => Text -> [InlineElem] -> r Text
wrapInSec content title = do
    richSecTitle <- htmlFold title
    let rawSecTitle = Text.concat . map extractRawText $ title
    --for the id, only use alphanum characters and hyphens, replacing spaces with hyphens.
    let secId = Text.replace " " "-" . Text.toLower . Text.strip . Text.filter (\x -> Char.isAlphaNum x || Char.isSpace x) $ rawSecTitle
    pure
      $ "<section id=\""
      `append` secId
      `append` "\">\n<h2>"
      `append` richSecTitle
      `append` "</h2>\n"
      `append` content
      `append` "</section>\n"
--2}}}

--1}}}

--how to turn IR to html. {{{1

instance Html Document where --{{{2
  htmlify (Document title sections) = do
      opts <- ask 
      --for each block element in document, turn it to html and append a newline. after that, combine list into one Text.
      richPageTitle <- htmlFold title
      let rawPageTitle = Text.concat . map extractRawText $ title
      --for the id, only use alphanum characters and hyphens, replacing spaces with hyphens.
      let pageId = Text.replace " " "-" . Text.toLower . Text.strip . Text.filter (\x -> Char.isAlphaNum x || Char.isSpace x) $ rawPageTitle
      content <- forM sections htmlify >>= pure . Text.concat
      css <- includeCss
      --combine everything for our final html page.
      pure
        $ "<!DOCTYPE HTML>\n" 
        `append` motd 
        `append` "<head>\n" 
        `append` meta
        `append` css
        `append` "<title>"
        `append` rawPageTitle
        `append` "</title>\n"
        `append` "</head>\n<html>\n<body>\n<article id=\""
        `append` pageId
        `append` "\">\n<h1>" 
        `append` richPageTitle
        `append` "</h1>\n"
        `append`  content 
        `append` "</article>\n</body>\n</html>\n"
--2}}}


instance Html Section where --{{{2
  htmlify (Section title elems) = do
      content <- forM elems (\x -> htmlify x >>= pure . (flip append "\n")) >>= pure . Text.concat
      maybe (pure content) (wrapInSec content) $ title

--2}}}

instance Html BlockElem where --{{{2
    htmlify (Paragraph inner) = wrap inner P
    htmlify (UnorderedList inner) = wrap inner UL
    htmlify (Image (AssetRef src)) = do
      opts <- ask
      let assetsDir = pack . toFilePath . oAssetsDir $ opts
      let assetPath = pack . SysPath.makeRelative "/" . unpack $ src
      pure $ "<img src=\"" `append` assetsDir `append` assetPath `append` "\" />"
    htmlify (Image (RemoteRef src)) = pure $ "<img src=\"" `append` src `append` "\" />"
    htmlify (CodeListing text) = pure $ "<pre class=\"code\">\n" `append` (makeSafe text) `append` "\n</pre>"
    htmlify (BlockQuote quote author) = do
      quoteText <- htmlFold quote
      authorText <- htmlFold author
      pure 
        $ "<div class=\"blockquote\">\n<span class=\"quote\">"
        `append` quoteText
        `append` "</span><span class=\"author\">"
        `append` authorText
        `append` "</span>\n</div>"

    htmlify (Group label elems) = do
      --for each block element in document, turn it to html and append a newline. after that, combine list into one Text.
      content <- forM elems (\x -> htmlify x >>= pure . (flip append "\n")) >>= pure . Text.concat
      pure $ "<div class=\"" `append` label `append` "\">\n" `append` content `append` "</div>"
      
--2}}}

instance Html InlineElem where --{{{2
    htmlify (Bold inner) = wrap inner Strong
    htmlify (Italic inner) = wrap inner Em
    htmlify (Verb inner) = pure $ "<code>" `append` (makeSafe inner) `append` "</code>"
    htmlify (PlainText inner) = pure . makeSafe $ inner 

    htmlify (Link title (PageRef url)) =  do
      titleText <- htmlFold title
      pure $ "<a class=\"local-page\" href=\"" `append` url `append` "\">" `append` titleText `append` "</a>"

    htmlify (Link title (AssetRef url)) = do
      opts <- ask
      let assetsDir = pack . toFilePath . oAssetsDir $ opts
      let assetPath = pack . SysPath.makeRelative "/" . unpack $ url
      titleText <- htmlFold title
      pure $ "<a class=\"local-asset\" href=\"" `append` assetsDir `append` assetPath `append` "\">" `append` titleText `append`"</a>"

    htmlify (Link title (RemoteRef url)) = do
      titleText <- htmlFold title
      pure $ "<a class=\"remote\" target=\"_blank\"  href=\"" `append` url `append` "\">" `append` titleText `append` "</a>"
--2}}}

instance Html UnorderedListItem where --{{{2
  htmlify (UnorderedListItem inner)  = wrap inner LI
--2}}}

--1}}}
