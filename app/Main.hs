{-# LANGUAGE QuasiQuotes #-}

module Main where

-- imports {{{1
import qualified Data.Map as Map
import qualified Data.Text.IO as TIO

import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as T (pack)

import qualified Toml
import Path

import Data.List (intercalate)
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import qualified System.FilePath as SysPath
import System.Directory
import System.Exit (die)
import Text.Printf (printf)
import Control.Monad

import Builder
import Options
import Error
--1}}}

--general todos:
--static checking link paths to make sure they exist
--make root index file user configurable
--make asset dir user configurable
--switch IO ops to text
--switch everything over to Path module
--use new options object

--build {{{1

--TODO: skip asset dir
getChildren :: Path Rel Dir -> IO [Path Rel Dir] --{{{2
getChildren dir = do
  dirContents <- listDirectory (toFilePath dir)
  children <- withCurrentDirectory (toFilePath dir) (filterM doesDirectoryExist dirContents)
  typedChildren <- mapM parseRelDir children
  return $ map ( \x -> dir </> x ) typedChildren
--2}}}

--TODO: make case insensitive, 
getKbyFiles :: Path Rel Dir -> IO [Path Rel File] --{{{2
getKbyFiles dir = do
  dirContents <- listDirectory (toFilePath dir)
  let kbyFiles = filter (\x -> SysPath.takeExtension x == ".kby") dirContents
  typedFileNames <- mapM parseRelFile kbyFiles
  return $ map ( \x -> dir </> x ) typedFileNames
--2}}}

build :: Options -> Path Rel Dir -> IO () --{{{2
build opts src = do
    let srcString = toFilePath src
    let homepage = oHomepageName opts
    input <- TIO.readFile $ srcString SysPath.</> homepage
    printf "Building %s...\n" homepage
    case kbyToHtml homepage input of
      Left err -> do
        let errType = case err of
                       (LexError _) -> "lexical" :: String
                       (ParseError _) -> "syntactic" :: String
        --TODO: print to stderr, gather and report errors at some point??
        printf "[ERROR] halting build of %s due to %s error:\n" srcString errType
        putStr $ unError err
      Right page -> writeFile ( toFilePath $ (oBuildDir opts)</>[relfile|index.html|] ) page
    children <- getChildren src
    files <- getKbyFiles src
    typedHomepage <- parseRelFile homepage >>= \x -> return $ src </> x
    mapM_ ( buildDir opts ) children
    mapM_ ( buildPage opts ) $ filter (/= typedHomepage) files
--2}}}

buildDir :: Options -> Path Rel Dir -> IO () --{{{2
buildDir opts src = do
  children <- getChildren src
  files <- getKbyFiles src
  mapM_ ( buildDir opts ) children
  mapM_ ( buildPage opts ) files
--2}}}

buildPage :: Options -> Path Rel File -> IO () --{{{2
buildPage opts src = do
  let srcString = toFilePath src
  (fileName,_) <- splitExtension $ filename src
  folderName <- parseRelDir $ toFilePath fileName
  dir <- replaceProperPrefix [reldir|src|] (oBuildDir opts) src >>= \x -> return $ parent x </> folderName
  createDirectoryIfMissing True (toFilePath dir)
  input <- TIO.readFile srcString
  printf "Building %s...\n" srcString
  case kbyToHtml srcString input of
    Left err -> do
      let errType = case err of
                      (LexError _) -> "lexical" :: String
                      (ParseError _) -> "syntactic" :: String
      printf "[ERROR] halting build of %s due to %s error:\n" srcString errType
      putStr $ unError err
    Right page -> writeFile ( toFilePath $ dir </> [relfile|index.html|] ) page
--2}}}

--1}}}

--actions {{{1

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
    , "build /path/to/source/dir:\t build a directory of .kby files to html."
    ]
--2}}}

--1}}}


--return map of flags and arguments from command.
parseFlags :: Map.Map String String -> [String] -> (Map.Map String String, [String]) --{{{2
parseFlags flags x@(flg:arg:xs) = case flg `elem` options of
                                      False -> (flags, x)
                                      True -> parseFlags newFlags xs
                                          where newFlags = Map.insert flg arg flags               
parseFlags flags x = (flags, x)
--2}}} 

--arg handlers. {{{1

--process command.
processCMD :: Options -> [String] -> IO () --{{{2
processCMD _  [] = putStrLn "No commands, nothing to do..."
processCMD _ ["help"] = help
processCMD opts ("build":arg:[]) = do
                            start <- getCPUTime
                            parseRelDir arg >>= build opts
                            end <- getCPUTime
                            let time = fromIntegral (end-start) / (10^12)
                              in printf "Finished in %0.4f sec.\n" (time :: Double)
processCMD _ x = printf "[ERROR] Command with too many arguments: \"%s\". Perhaps options passed out of order?\n" $ intercalate " " x
--2}}}

--todo: throw error if args after cmd
--1}}}

--entrypoint
main = do --{{{1
    args <- getArgs
    let (flags, cmd) = parseFlags Map.empty args
    let cmdOpts = partialOptionsFromFlags flags
    --TODO: check if file exists
    tomlRes <- Toml.decodeFileEither configCodec "kobayashi.toml"
    case tomlRes of
        Left errs -> do
          putStrLn "[ERROR] Error(s) in kobayashi.toml:"
          TIO.putStrLn $ Toml.prettyTomlDecodeErrors errs
        Right (Config tomlOpts) -> do
          let opts = makeOptions $ defaultPartialOptions <> tomlOpts <> cmdOpts
          case opts of
            Right options -> processCMD options cmd
            Left err -> do
              putStrLn "[ERROR] Error(s) in configuration:"
              print err
--1}}}
