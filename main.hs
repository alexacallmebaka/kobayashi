-- imports {{{1
import System.IO
import System.FilePath
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import Lexer (tokenize)
import Parser (parse)
import HTML (htmlify)
--1}}}

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
              printf "Building %s...\n" source
              createDirectoryIfMissing True "site"
              input <- readFile source
              case tokenize source input of
                Left err -> print err
                Right tokens -> case parse source tokens of
                  Left err -> print err
                  Right doc -> do
                    let content = concat $ map ((++ "\n") . htmlify) doc
                        page = "<!DOCTYPE HTML>\n<html>\n<body>\n" ++  content ++ "</body>\n</html>\n"
                        outfile = "site" </> source -<.> ".html"
                     in writeFile outfile page
--1}}}

--entrypoint + timing
main = do --{{{1
  start <- getCPUTime
  args <- getArgs
  processArgs args
  end <- getCPUTime
  let time = fromIntegral (end-start) / (10^12)
   in printf "Finished in %0.4f sec." (time :: Double)
--1}}}
