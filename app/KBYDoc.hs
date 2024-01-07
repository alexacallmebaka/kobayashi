module KBYDoc
    ( Document(..)
    , BlockElem(..)
    , InlineElem(..)
    , URL (..)
    , UnorderedListItem (..)
    ) where

import qualified Data.Text as T
import Path
import qualified System.FilePath as SysPath

import HTML
import Options

--types {{{1
type Document = [BlockElem]

data URL = RemoteRef { refSrc :: T.Text } 
         | PageRef { refSrc :: T.Text }
         | AssetRef { refSrc :: T.Text }
         deriving (Eq, Show, Ord)

data UnorderedListItem = UnorderedListItem [InlineElem] deriving (Eq, Show, Ord)

data BlockElem = Paragraph [InlineElem]
               | Header [InlineElem]
               | Subheader [InlineElem] 
               | UnorderedList [UnorderedListItem]
               | CodeListing [InlineElem]
               | Image URL deriving (Eq, Show, Ord)

--inline elements that represent rich text can be arbitrarily nested.
data InlineElem = Bold [InlineElem]
                | Italic [InlineElem]
                | Verb [InlineElem]
                | Link [InlineElem] URL
                | PlainText T.Text deriving (Eq, Show, Ord)
--1}}}

--how to turn doc to html.
instance HTML BlockElem where
    htmlify opts (Header inner) = wrap opts inner H1
    htmlify opts (Subheader inner) = wrap opts inner H2
    htmlify opts (Paragraph inner) = wrap opts inner P
    htmlify opts (UnorderedList inner) = wrap opts inner UL
    --need to somehow throw errors when i dont have an assetref here.
    --TODO: i would like to make asset dir user configurable
    htmlify opts (Image (AssetRef src)) = "<img src=\"" ++ ( toFilePath $ oAssetsDir opts ) ++ assetPath ++ "\">"
      where assetPath = SysPath.makeRelative "/" (T.unpack src)
    htmlify opts (Image (RemoteRef src)) = "<img src=\"" ++ ( T.unpack src ) ++ "\">"
    htmlify opts (CodeListing text) = wrap opts text Pre

instance HTML InlineElem where
    htmlify opts (Bold inner) = wrap opts inner Strong
    htmlify opts (Italic inner) = wrap opts inner Em
    htmlify opts (Verb inner) = wrap opts inner Code
    htmlify opts (Link title (PageRef url)) = wrap opts title (A $ T.unpack url)
    htmlify opts (Link title (AssetRef url)) = wrap opts title (A $ (toFilePath $ oAssetsDir opts) ++ assetPath )
      where assetPath = SysPath.makeRelative "/" (T.unpack url)
    htmlify opts (Link title (RemoteRef url)) = wrap opts title (A $ T.unpack url)
    htmlify opts (PlainText inner) = T.unpack inner 

instance HTML UnorderedListItem where
  htmlify opts (UnorderedListItem inner)  = wrap opts inner LI
