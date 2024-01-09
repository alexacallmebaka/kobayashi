--errors and related functions.

module Error
  ( 
    BuildError(..)
  , ErrorMsg

  , printErrors
  ) where

import Data.Text (Text, append)

import qualified Data.Text.IO as TIO


type ErrorMsg = Text

data BuildError = LexError { unError :: ErrorMsg }
                | ParseError { unError :: ErrorMsg }
                deriving (Eq, Show)

printErrors :: [BuildError] -> IO ()
printErrors [] = return ()
printErrors (err:errs) = do
  let errType = case err of  
                  LexError _ -> "[LEXICAL ERROR]"
                  ParseError _ -> "[PARSE ERROR]"
  let errMsg = errType `append` " " `append` unError err
  TIO.putStrLn errMsg 
  printErrors errs
