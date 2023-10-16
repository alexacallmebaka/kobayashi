module KBYDoc
    ( Document(..)
    , BlockElem(..)
    , InlineElem(..)
    , Scheme (..)
    , ResourceType (..)
    , URL (..)
    , UnorderedListItem (..)
    ) where

import qualified Data.Text as T
import HTML
import System.FilePath

--types {{{1
type Document = [BlockElem]

data Scheme = HTTP | HTTPS deriving (Eq, Show, Ord)

data ResourceType = KBY | JPEG | PNG deriving (Eq, Show, Ord)

data URL = RemoteRef { scheme :: Scheme, refSrc :: T.Text } 
         | LocalRef { rtype :: ResourceType, refSrc :: T.Text }
         deriving (Eq, Show, Ord)

data UnorderedListItem = UnorderedListItem [InlineElem] deriving (Eq, Show, Ord)

data BlockElem = Paragraph [InlineElem]
               | Header [InlineElem]
               | Subheader [InlineElem] 
               | UnorderedList [UnorderedListItem]
               | Image URL deriving (Eq, Show, Ord)

--inline elements that represent rich text can be arbitrarily nested.
data InlineElem = Bold [InlineElem]
                | Italic [InlineElem]
                | Link [InlineElem] URL
                | PlainText T.Text deriving (Eq, Show, Ord)
--1}}}

--how to turn doc to html.
instance HTML BlockElem where
    htmlify (Header inner) = wrap inner H1
    htmlify (Subheader inner) = wrap inner H2
    htmlify (Paragraph inner) = wrap inner P
    htmlify (UnorderedList inner) = wrap inner UL
    htmlify (Image url) = "<img src=\"" ++ (T.unpack $ refSrc url) ++ "\">"

instance HTML InlineElem where
    htmlify (Bold inner) = wrap inner Strong
    htmlify (Italic inner) = wrap inner Em
    htmlify (Link title (LocalRef KBY url)) = wrap title (A src)
      where src = (T.unpack url) -<.> ".html"
    htmlify (Link title (RemoteRef _ url)) = wrap title (A $ T.unpack url)
    htmlify (PlainText inner) = T.unpack inner 

instance HTML UnorderedListItem where
  htmlify (UnorderedListItem inner)  = wrap inner LI
