module Error
  ( BuildError(..)
  ) where

type ErrorMsg = String

data BuildError = LexError { unError :: ErrorMsg }
                | ParseError { unError :: ErrorMsg }
