module Main where

-- imports {{{1
import qualified Data.Map as Map
import qualified System.Directory.OsPath as OsPath
import qualified System.OsPath as Os
import qualified Data.Text as T (pack)

import Data.List (intercalate)
import System.IO
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import System.FilePath
import System.Directory
import Text.Printf (printf)

import Builder
import Error
--1}}}

--define args which operate on input.
dispatch :: Map.Map String (Map.Map String String -> String -> IO ()) --{{{1
dispatch = Map.fromList [("build", buildSite)]
--1}}}

getKbyFiles :: Os.OsPath -> IO [Os.OsPath]
getKbyFiles dir = do
  ext <- Os.encodeUtf ".kby"
  OsPath.listDirectory dir >>= \y -> return $ filter (\x -> ( Os.takeExtension x ) == ext ) y


buildPage' :: FilePath -> Os.OsPath -> IO ()
buildPage' outdir source = do
                 sourceString <- Os.decodeUtf source
                 input <- readFile sourceString
                 printf "Building %s...\n" sourceString
                 case kbyToHtml sourceString (T.pack input) of
                   Left err -> do
                           let errType = case err of
                                           (LexError _) -> "lexical"
                                           (ParseError _) -> "syntactic"
                           printf "[ERROR] halting build of %s due to %s error:\n" sourceString errType
                           putStr $ unError err
                   Right page -> do
                     ext <- Os.encodeUtf ".html"
                     outfile <- Os.decodeUtf $ Os.replaceExtension source ext
                     let outpath = ".." </> outdir </> outfile
                       in writeFile outpath page



--actions {{{1
buildPage :: FilePath -> FilePath -> IO () --{{{2
buildPage source outdir = do
                 input <- readFile source
                 printf "Building %s...\n" source
                 case kbyToHtml source (T.pack input) of
                   Left err -> do
                           let errType = case err of
                                           (LexError _) -> "lexical"
                                           (ParseError _) -> "syntactic"
                           printf "[ERROR] halting build of %s due to %s error:\n" source errType
                           putStr $ unError err
                   Right page -> do
                     let outfile = outdir </> (takeFileName source) -<.> ".html"
                       in writeFile outfile page
--2}}}

--takes directory
--run on root dir, recurse on all child dirs
--add on the new dirs ill need to make in build into odir path
buildSite :: Map.Map String String -> String -> IO () --{{{2
buildSite flags sourceString = do
    let odir = case Map.lookup "-odir" flags of
                Just custom -> custom
                Nothing -> "."
    createDirectoryIfMissing True odir
    --throw error if not directory somewhere in here
    start <- getCPUTime
    source <- Os.encodeUtf sourceString
    files <- getKbyFiles source
    OsPath.withCurrentDirectory source $ mapM_ ( buildPage' odir ) files
    end <- getCPUTime
    let time = fromIntegral (end-start) / (10^12)
      in printf "Finished in %0.4f sec.\n" (time :: Double)
--2}}}

options :: [String] --{{{2
options = ["-odir"]
--2}}}

help :: IO () --{{{2
help = mapM_ putStrLn [ "Usage: kobayashi [options] <command> \n"
    ,"options"
    , "============"
    , "-odir /path/to/output/directory:\t specify an output directory for the html files."
    ,"\ncommands"
    , "============"
    , "build /path/to/source.kby:\t build .kby file to html."
    ]
--2}}}
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
