module KBYDoc
    ( Document(..)
    , BlockElem(..)
    , InlineElem(..)
    , ResourceLocation (..)
    , ResourceType (..)
    , LinkSource (..)
    ) where

import qualified Data.Text as T
import HTML
import System.FilePath

--types {{{1
type Document = [BlockElem]

data ResourceLocation = Local | Remote deriving (Eq, Show, Ord)

data ResourceType = KBY | Other deriving (Eq, Show, Ord)

data LinkSource = LinkSource { location :: ResourceLocation
                             , resourceType :: ResourceType
                             , url :: T.Text
                             } deriving (Eq, Show, Ord)

data BlockElem = Paragraph [InlineElem]
                | Header [InlineElem]
                | Subheader [InlineElem] deriving (Eq, Show, Ord)

--inline elements that represent rich text can be arbitrarily nested.
data InlineElem = Bold [InlineElem]
                | Italic [InlineElem]
                | Link [InlineElem] LinkSource
                | PlainText T.Text deriving (Eq, Show, Ord)
--1}}}

--how to turn doc to html.
instance HTML BlockElem where
    htmlify (Header inner) = wrap inner H1
    htmlify (Subheader inner) = wrap inner H2
    htmlify (Paragraph inner) = wrap inner P

instance HTML InlineElem where
    htmlify (Bold inner) = wrap inner Strong
    htmlify (Italic inner) = wrap inner Em
    htmlify (Link title (LinkSource Local KBY url)) = wrap title (A src)
      where src = (T.unpack url ) -<.> ".html"
    htmlify (Link title (LinkSource _ _ url)) = wrap title (A $ T.unpack url)
    htmlify (PlainText inner) = T.unpack inner 
