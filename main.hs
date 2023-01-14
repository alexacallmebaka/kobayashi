import System.IO
import System.Environment
import Text.Parsec (ParseError)
import Control.Monad
import Lexer (tokenize)
import Parser (parse)
import HTML (htmlify)

--arg handler.
--dispatch :: [(String, String -> IO ())]

--first lex, then parse, then htmlify, then print html or errors.
buildPage :: String -> IO () --{{{1
buildPage source = do
              handle <- openFile source ReadMode
              contents <- hGetContents handle
              case tokenize source contents of
                Left l -> print l
                Right r -> case parse source r of
                  Left l -> print l
                  Right r -> do
                    putStrLn "<!DOCTYPE HTML>\n<html>\n<body>"
                    mapM_ (putStrLn . htmlify) r
                    putStrLn "</body>\n</html>"
              hClose handle 
--1}}}

main = do
  args <- getArgs
  buildPage "index.kby"
