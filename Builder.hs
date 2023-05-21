--build website from DocuElem.
module Builder
    ( buildPage
    ) where

import HTML (htmlify, DocuElem)

--htmlify list of DocuElem then print html or errors.
buildPage :: [DocuElem] -> String --{{{1
buildPage doc = "<!DOCTYPE HTML>\n<html>\n<body>\n" ++  content ++ "</body>\n</html>\n"
    where content = concat $ map ((++ "\n") . htmlify) doc
--1}}}
