-- imports {{{1
import qualified Data.Map as Map
import System.IO
import System.FilePath
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import Data.List (intercalate)
import Lexer (tokenize)
import Parser (parse)
import HTML (htmlify)
--1}}}

--define args which operate on input.
dispatch :: Map.Map String (Map.Map String String -> String -> IO ()) --{{{1
dispatch = Map.fromList [("build", buildPage)]
--1}}}

options :: [String] --{{{1
options = ["-odir"]
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

--process flags.
processFlags :: Map.Map String String -> [String] -> (Map.Map String String, [String]) --{{{1
processFlags flags x@(flg:arg:xs) = case flg `elem` options of
                                      False -> (flags, x)
                                      True -> processFlags newFlags xs
                                        where newFlags = Map.insert flg arg flags               
processFlags flags x = (flags, x)
--1}}} 

--process command.
processCMD :: Map.Map String String -> [String] -> IO () --{{{1
processCMD _  [] = putStrLn "No commands, nothing to do..."
processCMD _ ["help"] = help
processCMD opts (cmd:arg:[]) = case Map.lookup cmd dispatch of
                              Just action -> action opts arg
                              Nothing -> printf "[ERROR] halting due to invalid command: %s\n" cmd
processCMD _ x = printf "[ERROR] Command with too many arguments: \"%s\". Perhaps options passed out of order?\n" $ intercalate " " x
--1}}}

--lex, parse, htmlify, then print html or errors.
buildPage :: Map.Map String String -> String -> IO () --{{{1
buildPage flags source = do 
  start <- getCPUTime
  printf "Building %s...\n" source
  input <- readFile source
  case tokenize source input >>= parse source of
    Left err -> print err
    Right doc -> do
      dir <- return $ case Map.lookup "-odir" flags of
                        Just custom -> custom
                        Nothing -> "."
      createDirectoryIfMissing True dir
      let content = concat $ map ((++ "\n") . htmlify) doc
          page = "<!DOCTYPE HTML>\n<html>\n<body>\n" ++  content ++ "</body>\n</html>\n"
          outfile = dir </> (takeFileName source) -<.> ".html"
       in writeFile outfile page
  end <- getCPUTime
  let time = fromIntegral (end-start) / (10^12)
   in printf "Finished in %0.4f sec.\n" (time :: Double)
--1}}}

--todo: throw error if args after cmd

--entrypoint
main = do --{{{1
  args <- getArgs
  let (opts, cmd) = processFlags Map.empty args
  processCMD opts cmd
--1}}}
