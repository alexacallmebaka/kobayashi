--a module for the Html typeclass and related functions.

--pragmas {{{1

--used to make Tag derive the Generic typeclass, so then it can have a Hashable instance.
{-# LANGUAGE DeriveGeneric #-}

--used to partially apply the MonadReader constraint to only types with "Options" as the environment type.
{-# LANGUAGE FlexibleContexts #-}

--1}}}

--exports {{{1
module Html 
    ( 
      Html(..)
    , Tag(..)

    , meta
    , motd
    , includeCss
    , includeFavicon

    , genTags
    , htmlFold
    , wrap
    ) where
--1}}}

--imports {{{1
import Control.Monad (foldM, forM)
import Control.Monad.Reader (ask, MonadReader)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Text (append, pack, Text, unpack)
import GHC.Generics (Generic)
import Path (toFilePath)

import Options(Options(..))

import qualified Data.Char as Char
import qualified Data.HashSet as HS
import qualified Data.Text as Text
import qualified System.FilePath as SysPath

import qualified Document as IR
--1}}}

{-typeclass for things that can be turned into html-encoded text.
the MonadReader will hold enviorment information such as the assets directory to use when creating image tags. 
-}
class Html a where --{{{1
    htmlify :: (MonadReader Options r ) => a -> r Text
--1}}}

--html tag data type, related functions and instances {{{1
--html tag data type {{{2
data Tag = Strong
         | Em
         | H1
         | H2
         | UL
         | LI
         | P 
         deriving (Generic, Eq)
--2}}}

--make Tag hashable using GHC's generics.
instance Hashable Tag

--turn a tag into text!
textify :: Tag -> Text --{{{2
textify Strong = "strong"
textify Em = "em"
textify H1 = "h1"
textify H2 = "h2"
textify UL = "ul"
textify LI = "li"
textify P = "p"
--2}}}

--set with tags that take up thier own line (i.e. are followed by newline).
standaloneTags :: HS.HashSet Tag --{{{2
standaloneTags = HS.fromList [P, UL, LI]
--2}}}

--check if tag is in standalone set.
isStandalone :: Tag -> Bool --{{{2
isStandalone x = HS.member x standaloneTags
--2}}}

--generate start and end tag text from a tag datatype.
genTags :: Tag -> (Text, Text) --{{{2
genTags UL  = ("<ul>\n", "</ul>")
genTags LI  = ("<li>\n", "\n</li>\n")
genTags x = ("<" `append` tag `append` ">" `append` nline, nline `append` "</" `append` tag `append` ">")
            where tag = textify x
                  --certain tags should be formatted on their own line.
                  nline = if isStandalone x then "\n"  else ""
--2}}}

--1}}}

--utility functions for creating html-encoded text. {{{1

--fold up a list of html elements into an HTML string.
htmlFold :: (Html a, MonadReader Options r) => [a] -> r Text --{{{2
--for the lambda: turn element to html and then append that text to the accumulator.
htmlFold = foldM (\acc x -> htmlify x >>= pure . append acc) ""
--2}}}

--wrap a list of inner html in tags from outer html.
wrap :: (Html a, MonadReader Options r) => [a] -> Tag -> r Text --{{{2
wrap inner tag = do
              let (open, close) = genTags tag   
              content <- (htmlFold inner) 
              pure $ open `append` content `append` close
--2}}}

--generate a tag to include css.
includeCss :: (MonadReader Options r) => r Text --{{{2
includeCss = do
  opts <- ask
  let cssPath = pack . toFilePath . oCssPath $ opts
  pure $ "<link rel=\"stylesheet\" href=\"" `append` cssPath `append` "\" />\n"
--2}}}

--generate a tag to include favicon.
includeFavicon :: (MonadReader Options r) => r Text --{{{2
includeFavicon = do
  opts <- ask
  let faviconPath = pack . toFilePath . oFaviconPath $ opts
  pure 
    $ "<link rel=\"icon\" type=\"image/x-icon\" href=\"" `append` faviconPath `append` "\" />\n"
    `append` "<link rel=\"shortcut icon\" type=\"image/x-icon\" href=\"" `append` faviconPath `append` "\" />\n"
--2}}}

includeNavbar :: (MonadReader Options r) => r Text --{{{2
includeNavbar = do
  opts <- ask
  case oNavbar opts of
    [] -> pure ""
    navbar -> do
      --this is a sloppy solution, but i was having trouble with tomland making an ordered list from toml.
      contents <- forM navbar (\x -> htmlify x >>= pure . (flip append "</li>\n") . (append "<li>")) >>= pure . Text.concat
      pure $ "<nav>\n<ul>\n" `append` contents `append` "</ul>\n</nav>"


--comment to add some flair.
motd :: Text --{{{2
motd = "<!--Made with ❤️ using Kobayashi: https://github.com/alexacallmebaka/kobayashi-->\n"
--2}}}

--add various meta tags.
meta :: Text
--sets encoding, viewport.
meta = "<meta charset=\"UTF-8\" />\n<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\" />\n"

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
extractRawText :: IR.InlineElem -> Text
extractRawText (IR.Verb txt) = txt
extractRawText (IR.PlainText txt) = txt
extractRawText (IR.Link title _ ) = Text.concat . map extractRawText $ title
extractRawText ( IR.Bold txt ) = Text.concat . map extractRawText $ txt
extractRawText ( IR.Italic txt ) = Text.concat . map extractRawText $ txt
--2}}}

--wrap HTML-encoded text in a section with a given title {{{2
wrapInSec :: ( MonadReader Options r ) => Text -> [IR.InlineElem] -> r Text
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

instance Html IR.Document where --{{{2
  htmlify (IR.Document title sections) = do
      opts <- ask 
      --for each block element in document, turn it to html and append a newline. after that, combine list into one Text.
      richPageTitle <- htmlFold title
      let rawPageTitle = Text.concat . map extractRawText $ title
      --for the id, only use alphanum characters and hyphens, replacing spaces with hyphens.
      let pageId = Text.replace " " "-" . Text.toLower . Text.strip . Text.filter (\x -> Char.isAlphaNum x || Char.isSpace x) $ rawPageTitle
      content <- forM sections htmlify >>= pure . Text.concat
      css <- includeCss
      favicon <- includeFavicon
      navbar <- includeNavbar
      --combine everything for our final html page.
      pure
        $ "<!DOCTYPE HTML>\n" 
        `append` motd 
        `append` "<head>\n" 
        `append` meta
        `append` css
        `append` favicon
        `append` "<title>"
        `append` rawPageTitle
        `append` "</title>\n"
        `append` "</head>\n<html>\n<body>\n<article id=\""
        `append` pageId
        `append` "\">\n<h1>" 
        `append` richPageTitle
        `append` "</h1>\n"
        `append` navbar
        `append`  content 
        `append` "</article>\n</body>\n</html>\n"
--2}}}

instance Html IR.Section where --{{{2
  htmlify (IR.Section title elems) = do
      content <- forM elems (\x -> htmlify x >>= pure . (flip append "\n")) >>= pure . Text.concat
      maybe (pure content) (wrapInSec content) $ title

--2}}}

instance Html IR.BlockElem where --{{{2
    htmlify (IR.Paragraph inner) = wrap inner P
    htmlify (IR.UnorderedList inner) = wrap inner UL
    htmlify (IR.Image (IR.AssetRef src)) = do
      opts <- ask
      let assetsDir = pack . toFilePath . oAssetsDir $ opts
      let assetPath = pack . SysPath.makeRelative "/" . unpack $ src
      pure $ "<img src=\"" `append` assetsDir `append` assetPath `append` "\" />"
    htmlify (IR.Image (IR.RemoteRef src)) = pure $ "<img src=\"" `append` src `append` "\" />"
    htmlify (IR.CodeListing text) = pure $ "<pre>\n<code>\n" `append` (makeSafe text) `append` "\n</code>\n</pre>"
    htmlify (IR.BlockQuote quote author) = do
      quoteText <- htmlFold quote
      authorText <- htmlFold author
      pure 
        $ "<div class=\"blockquote\">\n<span class=\"quote\">"
        `append` quoteText
        `append` "</span><span class=\"author\">"
        `append` authorText
        `append` "</span>\n</div>"

    htmlify (IR.Group label elems) = do
      --for each block element in document, turn it to html and append a newline. after that, combine list into one Text.
      content <- forM elems (\x -> htmlify x >>= pure . (flip append "\n")) >>= pure . Text.concat
      pure $ "<div class=\"" `append` label `append` "\">\n" `append` content `append` "</div>"
      
--2}}}

instance Html IR.InlineElem where --{{{2
    htmlify (IR.Bold inner) = wrap inner Strong
    htmlify (IR.Italic inner) = wrap inner Em
    htmlify (IR.Verb inner) = pure $ "<code>" `append` (makeSafe inner) `append` "</code>"
    htmlify (IR.PlainText inner) = pure . makeSafe $ inner 

    htmlify (IR.Link title (IR.PageRef url)) =  do
      titleText <- htmlFold title
      pure $ "<a class=\"local-page\" href=\"" `append` url `append` "\">" `append` titleText `append` "</a>"

    htmlify (IR.Link title (IR.AssetRef url)) = do
      opts <- ask
      let assetsDir = pack . toFilePath . oAssetsDir $ opts
      let assetPath = pack . SysPath.makeRelative "/" . unpack $ url
      titleText <- htmlFold title
      pure $ "<a class=\"local-asset\" href=\"" `append` assetsDir `append` assetPath `append` "\">" `append` titleText `append`"</a>"

    htmlify (IR.Link title (IR.RemoteRef url)) = do
      titleText <- htmlFold title
      pure $ "<a class=\"remote\" target=\"_blank\"  href=\"" `append` url `append` "\">" `append` titleText `append` "</a>"
--2}}}

instance Html IR.UnorderedListItem where --{{{2
  htmlify (IR.UnorderedListItem inner)  = wrap inner LI
--2}}}

--1}}}
