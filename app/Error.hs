--errors and related functions.

--exports {{{1
module Error
  ( 
    BuildError(..)
  , ErrorMsg

  , printErrors
  ) where
--1}}}

--imports {{{1
import Data.Text (Text, append)

import qualified Data.Text.IO as TIO
--1}}}

--data types and aliases {{{1
type ErrorMsg = Text

data BuildError --{{{2
                = LexError { unError :: ErrorMsg } 
                | ParseError { unError :: ErrorMsg } 
                deriving (Eq, Show)
--2}}}

--1}}}

printErrors :: [BuildError] -> IO () --{{{1
printErrors [] = pure ()
printErrors (err:errs) = do
  let errType = case err of  
                  LexError _ -> "[LEXICAL ERROR]"
                  ParseError _ -> "[PARSE ERROR]"
  let errMsg = errType `append` " " `append` unError err
  TIO.putStrLn errMsg 
  printErrors errs
--1}}}
