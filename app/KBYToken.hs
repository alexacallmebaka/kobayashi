module KBYToken
    ( KBYToken(..)
    ) where

import qualified Data.Text as T

data KBYToken = BeginHeader
              | BeginSubheader
              | Bold 
              | Italic 
              | TextChar T.Text deriving (Eq, Show)
