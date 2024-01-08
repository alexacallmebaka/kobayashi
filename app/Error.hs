module Error
  ( BuildError(..)
  , ErrorMsg
  ) where

import Data.Text

type ErrorMsg = Text

data BuildError = LexError { unError :: ErrorMsg }
                | ParseError { unError :: ErrorMsg }
