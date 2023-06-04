--build website from DocuElem.
module Builder
    ( buildPage
    ) where

import Text.Parsec (ParseError, SourceName)
import HTML (htmlify, DocuElem)
import Lexer (tokenize)
import Parser (parse)

--htmlify list of DocuElem then print html or errors.
toHTML :: [DocuElem] -> String --{{{1
toHTML doc = "<!DOCTYPE HTML>\n<html>\n<body>\n" ++  content ++ "</body>\n</html>\n"
    where content = concat $ map ((++ "\n") . htmlify) doc
--1}}}

-- kby => html.
buildPage :: SourceName -> String -> Either ParseError String --{{{1
buildPage source input = 
    case tokenize source input >>= parse source of
        Left err -> Left err
        Right doc -> Right $ toHTML doc
--1}}}
