module Document
    ( Document(..)
    , BlockElem(..)
    , InlineElem(..)
    , URL (..)
    , UnorderedListItem (..)
    ) where

import Data.Text
import Control.Monad.Reader
import Path
import qualified System.FilePath as SysPath

import HTML
import Options

--types {{{1
type Document = [BlockElem]

data URL = RemoteRef { refSrc :: Text } 
         | PageRef { refSrc :: Text }
         | AssetRef { refSrc :: Text }
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
                | PlainText Text deriving (Eq, Show, Ord)
--1}}}

--how to turn doc to html.
instance HTML BlockElem where
    htmlify (Header inner) = wrap inner H1
    htmlify (Subheader inner) = wrap inner H2
    htmlify (Paragraph inner) = wrap inner P
    htmlify (UnorderedList inner) = wrap inner UL
    htmlify (Image (AssetRef src)) = do
          opts <- ask
          let assetsDir = pack . toFilePath . oAssetsDir $ opts
          let assetPath = pack . SysPath.makeRelative "/" . unpack $ src
          return $ "<img src=\"" `append` assetsDir `append` assetPath `append` "\">"
    htmlify (Image (RemoteRef src)) = return $ "<img src=\"" `append` src `append` "\">"
    htmlify (CodeListing text) = wrap text Pre

instance HTML InlineElem where
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
    htmlify (PlainText inner) = return $ inner 

instance HTML UnorderedListItem where
  htmlify (UnorderedListItem inner)  = wrap inner LI
