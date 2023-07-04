module Token
    ( Token(..)
    ) where

import qualified Data.Text as T

data Token = Header | Subheader | Bold | Italic | EOL | Break | TextChar T.Text deriving (Eq, Show)
