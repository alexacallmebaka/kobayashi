--kobayashi's flat intermediate representation.

--exports {{{1
module Document
    (
      BlockElem(..)
    , Document(..)
    , InlineElem(..)
    , UnorderedListItem (..)
    , Url (..)
    ) where
--1}}}

--imports {{{1
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, ask)
import Data.Text (append, pack, Text, unpack)
import Path (toFilePath)

import Html (Html, htmlify, includeCss, motd, Tag(..), wrap)
import Options (Options(..))

import qualified Data.Text as Text
import qualified System.FilePath as SysPath
--1}}}


--types {{{1
newtype Document = Document { unDocument :: [BlockElem] }

data Url = RemoteRef { refSrc :: Text }
         | PageRef { refSrc :: Text }
         | AssetRef { refSrc :: Text }
         deriving (Eq, Show, Ord)

data UnorderedListItem = UnorderedListItem [InlineElem] deriving (Eq, Show, Ord)

data BlockElem = Paragraph [InlineElem]
               | Header [InlineElem]
               | Subheader [InlineElem] 
               | UnorderedList [UnorderedListItem]
               | CodeListing [InlineElem]
               | Image Url deriving (Eq, Show, Ord)

--inline elements that represent rich text can be arbitrarily nested.
data InlineElem = Bold [InlineElem]
                | Italic [InlineElem]
                | Verb [InlineElem]
                | Link [InlineElem] Url
                | PlainText Text deriving (Eq, Show, Ord)
--1}}}

--how to turn IR to html. {{{1

$instance Html Document where --{{{2
  htmlify (Document elems) = do
      opts <- ask 
      --for each block element in document, turn it to html and append a newline. after that, combine list into one Text.
      content <- forM elems (\x -> htmlify x >>= pure . append "\n") >>= pure . Text.concat
      css <- includeCss
      --combine everything for our final html page.
      pure
        $ "<!DOCTYPE HTML>\n" 
        `append` motd 
        `append` "<head>\n" 
        `append` css
        `append`  "</head>\n<html>\n<body>\n" 
        `append`  content 
        `append` "</body>\n</html>\n"
--2}}}

instance Html BlockElem where --{{{2
    htmlify (Header inner) = wrap inner H1
    htmlify (Subheader inner) = wrap inner H2
    htmlify (Paragraph inner) = wrap inner P
    htmlify (UnorderedList inner) = wrap inner UL
    htmlify (Image (AssetRef src)) = do
          opts <- ask
          let assetsDir = pack . toFilePath . oAssetsDir $ opts
          let assetPath = pack . SysPath.makeRelative "/" . unpack $ src
          pure $ "<img src=\"" `append` assetsDir `append` assetPath `append` "\">"
    htmlify (Image (RemoteRef src)) = pure $ "<img src=\"" `append` src `append` "\">"
    htmlify (CodeListing text) = wrap text Pre
--2}}}

instance Html InlineElem where --{{{2
    htmlify (Bold inner) = wrap inner Strong
    htmlify (Italic inner) = wrap inner Em
    htmlify (Verb inner) = wrap inner Code
    htmlify (Link title (PageRef url)) = wrap title (A url)
    htmlify (Link title (AssetRef url)) = do
      opts <- ask
      let assetsDir = pack . toFilePath . oAssetsDir $ opts
      let assetPath = pack . SysPath.makeRelative "/" . unpack $ url
      wrap title ( A $ assetsDir `append`  assetPath )
    htmlify (Link title (RemoteRef url)) = wrap title (A url)
    htmlify (PlainText inner) = pure $ inner 
--2}}}

instance Html UnorderedListItem where --{{{2
  htmlify (UnorderedListItem inner)  = wrap inner LI
--2}}}

--1}}}
