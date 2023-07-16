module KBYDoc
    ( Document(..)
    , BlockElem(..)
    , InlineElem(..)
    ) where

import qualified Data.Text as T

--types {{{1
type Document = [BlockElem]

data BlockElem = Paragraph [InlineElem]
                | Header [InlineElem ]
                | Subheader [InlineElem] deriving (Eq, Show, Ord)

--inline elements that represent rich text can be arbitrarily nested.
data InlineElem = Bold [InlineElem]
                | Italic [InlineElem]
                | PlainText T.Text deriving (Eq, Show, Ord)
--1}}}
