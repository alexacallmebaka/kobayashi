import System.IO
import System.Environment
import Lexer (tokenize)
import Parser (parse)
import HTML (htmlify)
import Data.List

--define args which operate on input.
dispatch :: [(String, [String] -> IO ())] --{{{1
dispatch = [ ("build", buildPage . head) ]
--1}}}

--help command.
help :: IO () --{{{1
help = mapM_ putStrLn [ "Usage: kobayashi <command>\n"
                      ,"commands"
                      , "============"
                      , "build /path/to/source.kby:\t build .kby file to html."
                      ]
--1}}}

--arg handler.
processArgs :: [String] -> IO () --{{{1
processArgs [] = putStrLn "No commands, nothing to do..."
processArgs ["help"] = help
processArgs (cmd:args) = case lookup cmd dispatch of
                      Just action -> action args
                      Nothing -> putStrLn $ "[ERROR] halting due to invalid command: " ++ cmd
--1}}}

--lex, parse, htmlify, then print html or errors.
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
  processArgs args
