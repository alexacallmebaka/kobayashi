module KBYDoc
    ( Document(..)
    , BlockElem(..)
    , InlineElem(..)
    ) where

import qualified Data.Text as T
import HTML

--types {{{1
type Document = [BlockElem]

data BlockElem = Paragraph [InlineElem]
                | Header [InlineElem]
                | Subheader [InlineElem] deriving (Eq, Show, Ord)

--inline elements that represent rich text can be arbitrarily nested.
data InlineElem = Bold [InlineElem]
                | Italic [InlineElem]
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
    htmlify (PlainText inner) = T.unpack inner 
