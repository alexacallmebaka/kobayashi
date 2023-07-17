--build website from DocuElem.
module Builder
    ( buildPage
    ) where

import qualified Data.Text as T
import Data.List
import Data.Void

import Text.Megaparsec.Error

import HTML
import KBYDoc
import KBYToken
import Lexer
import Parser

type BuildError = String
type SourceName = String

--htmlify internal Document.
toHTML :: Document -> String --{{{1
toHTML doc = "<!DOCTYPE HTML>\n<html>\n<body>\n" ++  content ++ "</body>\n</html>\n"
    where content = concatMap ((++ "\n") . htmlify) doc
--1}}}

--lex a file and return a stream of tokens or an error as a string.
lexFile :: SourceName -> T.Text -> Either BuildError KBYStream
lexFile source input = case tokenize source input of
        Left err -> Left $ errorBundlePretty err
        Right tokens -> Right tokens

--parse a file and return a stream of tokens or an error as a string.
parseFile :: SourceName -> KBYStream -> Either BuildError Document
parseFile source input = case parseTokens source input of
        Left err -> Left $ errorBundlePretty err
        Right doc -> Right doc


-- kby => html.
buildPage :: SourceName -> T.Text -> Either BuildError String --{{{1
buildPage source input = 
    case (lexFile source input) >>= (parseFile source) of
        Right doc -> Right $ toHTML doc
        Left x -> Left x
--1}}}
