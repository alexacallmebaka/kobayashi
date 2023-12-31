module KBYDoc
    ( Document(..)
    , BlockElem(..)
    , InlineElem(..)
    , URL (..)
    , UnorderedListItem (..)
    ) where

import qualified Data.Text as T

import HTML

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
    htmlify (Header inner) = wrap inner H1
    htmlify (Subheader inner) = wrap inner H2
    htmlify (Paragraph inner) = wrap inner P
    htmlify (UnorderedList inner) = wrap inner UL
    --need to somehow throw errors when i dont have an assetref here.
    --TODO: i would like to make asset dir user configurable
    htmlify (Image (AssetRef src)) = "<img src=\"/assets" ++ ( T.unpack src ) ++ "\">"
    htmlify (Image (RemoteRef src)) = "<img src=\"" ++ ( T.unpack src ) ++ "\">"
    htmlify (CodeListing text) = wrap text Pre

instance HTML InlineElem where
    htmlify (Bold inner) = wrap inner Strong
    htmlify (Italic inner) = wrap inner Em
    htmlify (Verb inner) = wrap inner Code
    htmlify (Link title (PageRef url)) = wrap title (A $ T.unpack url)
    htmlify (Link title (AssetRef url)) = wrap title (A $ "/assets" ++ (T.unpack url) )
    htmlify (Link title (RemoteRef url)) = wrap title (A $ T.unpack url)
    htmlify (PlainText inner) = T.unpack inner 

instance HTML UnorderedListItem where
  htmlify (UnorderedListItem inner)  = wrap inner LI
