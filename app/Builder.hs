--build website from DocuElem.
module Builder
    ( toHTML
    ) where

import HTML
import KBYDoc
import Lexer
import Parser
import Data.List

--htmlify list of DocuElem then print html or errors.
toHTML :: Document -> String --{{{1
toHTML doc = "<!DOCTYPE HTML>\n<html>\n<body>\n" ++  content ++ "</body>\n</html>\n"
    where content = concatMap ((++ "\n") . htmlify) doc
--1}}}

-- kby => html.
--buildPage :: SourceName -> String -> Either ParseErrorBundle String --{{{1
--buildPage source input = 
--    case tokenize source input >>= parse source of
--        Left err -> Left err
--        Right doc -> Right $ toHTML doc
--1}}}
