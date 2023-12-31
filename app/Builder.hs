--build website from DocuElem.
module Builder
    ( kbyToHtml
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
import Error

type SourceName = String

motd :: String
motd = "<!--Made with <3 using Kobayashi: https://github.com/alexacallmebaka/kobayashi-->\n"

css :: String
css = "<link rel=\"stylesheet\" href=\"/assets/style.css\">\n"

--htmlify internal Document.
toHTML :: Document -> String --{{{1
toHTML doc = "<!DOCTYPE HTML>\n" ++ motd ++ "<head>\n"++ css ++ "</head>\n<html>\n<body>\n" ++  content ++ "</body>\n</html>\n"
    where content = concatMap ((++ "\n") . htmlify) doc
--1}}}

--lex a file and return a stream of tokens or an error as a string.
lexFile :: SourceName -> T.Text -> Either BuildError KBYStream
lexFile source input = case tokenize source input of
        Left err -> Left . LexError $ errorBundlePretty err
        Right tokens -> Right tokens

--parse a file and return a stream of tokens or an error as a string.
parseFile :: SourceName -> KBYStream -> Either BuildError Document
parseFile source input = case parseTokens source input of
        Left err -> Left . ParseError $ errorBundlePretty err
        Right doc -> Right doc


-- kby => html.
kbyToHtml :: SourceName -> T.Text -> Either BuildError String --{{{1
kbyToHtml source input = 
    case (lexFile source input) >>= (parseFile source) of
        Right doc -> Right $ toHTML doc
        Left x -> Left x
--1}}}
