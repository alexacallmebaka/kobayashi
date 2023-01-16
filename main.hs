-- imports {{{1
import qualified Data.Map as Map
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
dispatch :: Map.Map String (Map.Map String String -> [String] -> IO ()) --{{{1
dispatch = Map.fromList [("build", \x y-> buildPage x $ head y)]
--1}}}

--flag default values.
defaults :: Map.Map String String --{{{1
defaults = Map.fromList [("-odir",".")]
--1}}}

--help command.
help :: IO () --{{{1
help = mapM_ putStrLn [ "Usage: kobayashi [options] <command> \n"
                      ,"options"
                      , "============"
                      , "-odir /path/to/output/directory:\t specify an output directory for the html files."
                      ,"\ncommands"
                      , "============"
                      , "build /path/to/source.kby:\t build .kby file to html."
                      ]
--1}}}

--arg handlers.

--process flags 
processFlags :: Map.Map String String -> [String] -> (Map.Map String String, [String]) --{{{1
processFlags flags x@(flg:arg:xs) = case Map.lookup flg flags of
                                      Nothing -> (flags, x)
                                      Just _ -> processFlags newFlags xs
                                        where newFlags = Map.insert flg arg flags               
processFlags flags x = (flags, x)
--1}}} 


processCMD :: Map.Map String String -> [String] -> IO () --{{{1
processCMD _  [] = putStrLn "No commands, nothing to do..."
processCMD _ ["help"] = help
processCMD opts (cmd:args) = case Map.lookup cmd dispatch of
                              Just action -> action opts args
                              Nothing -> putStrLn $ printf "[ERROR] halting due to invalid command: %s\n" cmd
--1}}}

--lex, parse, htmlify, then print html or errors.
buildPage :: Map.Map String String -> String -> IO () --{{{1
buildPage flags source = do 
              printf "Building %s...\n" source
              input <- readFile source
              case tokenize source input of
                Left err -> print err
                Right tokens -> case parse source tokens of
                  Left err -> print err
                  Right doc -> case Map.lookup "-odir" flags of
                    Just dir -> do
                      createDirectoryIfMissing True dir
                      let content = concat $ map ((++ "\n") . htmlify) doc
                          page = "<!DOCTYPE HTML>\n<html>\n<body>\n" ++  content ++ "</body>\n</html>\n"
                          outfile = dir </> source -<.> ".html"
                       in writeFile outfile page
                    Nothing -> putStrLn "[ERROR] no output directory specified. Please run \"kobayashi help\" for options."
--1}}}

--entrypoint + timing
main = do --{{{1
  start <- getCPUTime
  args <- getArgs
  let (opts, cmd) = processFlags defaults args
  processCMD opts cmd
  end <- getCPUTime
  let time = fromIntegral (end-start) / (10^12)
   in printf "Finished in %0.4f sec.\n" (time :: Double)
--1}}}
