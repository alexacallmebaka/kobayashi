module Error
  ( BuildError(..)
  , ErrorMsg
  ) where

type ErrorMsg = String

data BuildError = LexError { unError :: ErrorMsg }
                | ParseError { unError :: ErrorMsg }
