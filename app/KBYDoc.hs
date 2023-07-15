module KBYDoc
    ( Document(..)
    , BlockElem(..)
    , InlineElem(..)
    ) where

import qualified Data.Text as T

type Document = [BlockElem]

data BlockElem = Paragraph [InlineElem]
                | Header [InlineElem ]
                | Subheader [InlineElem] deriving (Eq, Show, Ord)

data InlineElem = Bold [InlineElem]
                | Italic [InlineElem]
                | PlainText T.Text deriving (Eq, Show, Ord)
