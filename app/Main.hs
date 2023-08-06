module Main where

-- imports {{{1
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Text as T (pack)
import System.IO
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import System.FilePath
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import Builder
--1}}}

--define args which operate on input.
dispatch :: Map.Map String (Map.Map String String -> String -> IO ()) --{{{1
dispatch = Map.fromList [("build", buildSite)]
--1}}}

--actions {{{1
buildSite :: Map.Map String String -> String -> IO () --{{{1
buildSite flags source = do
    input <- readFile source
    let dir = case Map.lookup "-odir" flags of
                Just custom -> custom
                Nothing -> "."
    createDirectoryIfMissing True dir
    start <- getCPUTime
    printf "Building %s...\n" source
    case buildPage source (T.pack input) of
      Left err -> do
          putStrLn "[ERROR] halting due to a build-time error:"
          putStr err
      Right page -> do
        let outfile = dir </> (takeFileName source) -<.> ".html"
            in writeFile outfile page
        end <- getCPUTime
        let time = fromIntegral (end-start) / (10^12)
            in printf "Finished in %0.4f sec.\n" (time :: Double)
--1}}}

options :: [String] --{{{1
options = ["-odir"]
--1}}}

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

--arg handlers. {{{1

--process flags.
processFlags :: Map.Map String String -> [String] -> (Map.Map String String, [String]) --{{{2
processFlags flags x@(flg:arg:xs) = case flg `elem` options of
                                      False -> (flags, x)
                                      True -> processFlags newFlags xs
                                          where newFlags = Map.insert flg arg flags               
processFlags flags x = (flags, x)
--2}}} 

--process command.
processCMD :: Map.Map String String -> [String] -> IO () --{{{2
processCMD _  [] = putStrLn "No commands, nothing to do..."
processCMD _ ["help"] = help
processCMD opts (cmd:arg:[]) = case Map.lookup cmd dispatch of
                                    Just action -> action opts arg
                                    Nothing -> printf "[ERROR] halting due to invalid command: %s\n" cmd
processCMD _ x = printf "[ERROR] Command with too many arguments: \"%s\". Perhaps options passed out of order?\n" $ intercalate " " x
--2}}}

--todo: throw error if args after cmd
--1}}}

--entrypoint
main = do --{{{1
    args <- getArgs
    let (opts, cmd) = processFlags Map.empty args
    processCMD opts cmd
--1}}}
