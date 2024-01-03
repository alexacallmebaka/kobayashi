module Main where

-- imports {{{1
import qualified Data.Map as Map
import qualified System.Directory.OsPath as OsPath
import qualified System.OsPath as Os
import qualified Data.Text.IO as TIO

import Data.Text (Text)
import qualified Data.Text as T (pack)

import Toml (TomlCodec, (.=))
import qualified Toml

import Data.List (intercalate)
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import System.FilePath
import System.Directory
import Text.Printf (printf)
import Control.Monad

import Builder
import Error
--1}}}

--general todos:
--static checking link paths to make sure they exist
--make root index file user configurable
--make asset dir user configurable
---switch IO ops to text

--modeling our toml file
data Config = Config { project :: Project }

data Project = Project
  { buildDir :: Text
  , assetsDir :: Text
  , cssPath :: Text
  , homepageName :: Text
  }


configCodec :: TomlCodec Config
configCodec = Config <$> Toml.table projectConfig "project" .= project

projectConfig :: TomlCodec Project
projectConfig = Project
  <$> Toml.text "build_dir" .= buildDir
  <*> Toml.text "assets_dir" .= assetsDir
  <*> Toml.text "css_path" .= cssPath
  <*> Toml.text "homepage_name" .= homepageName

--define args which operate on input.
dispatch :: Map.Map String (Map.Map String String -> String -> IO ()) --{{{1
dispatch = Map.fromList [("build", build)]
--1}}}

--build {{{1

--TODO: skip asset dir
getChildren :: Os.OsPath -> IO [Os.OsPath] --{{{2
getChildren dir = OsPath.listDirectory dir >>= filterM ( \x -> OsPath.doesDirectoryExist $ Os.combine dir x ) 
--2}}}

--TODO: make case insensitive, 
getKbyFiles :: Os.OsPath -> IO [Os.OsPath] --{{{2
getKbyFiles dir = do
  ext <- Os.encodeUtf ".kby"
  OsPath.listDirectory dir >>= \y -> return $ filter (\x -> ( Os.takeExtension x ) == ext ) y
--2}}}

--takes directory
build :: Map.Map String String -> String -> IO () --{{{2
build flags sourceString = do
    odir <- case Map.lookup "-odir" flags of
               Just custom -> Os.encodeUtf custom >>= OsPath.makeAbsolute
               Nothing -> OsPath.getCurrentDirectory
    source <- Os.encodeUtf sourceString
    start <- getCPUTime
    rootIndex <- Os.encodeUtf "index.html"
    --error handling for if index doesnt exist
    outpath <- Os.decodeUtf odir >>= \x -> return $ x </> "index.html"
    input <- readFile $ sourceString </> "index.kby"
    printf "Building %s...\n" (dropFileName outpath)
    case kbyToHtml sourceString (T.pack input) of
      Left err -> do
        let errType = case err of
                       (LexError _) -> "lexical" :: String
                       (ParseError _) -> "syntactic" :: String
        printf "[ERROR] halting build of %s due to %s error:\n" (dropFileName outpath) errType
        putStr $ unError err
      Right page -> writeFile outpath page
    children <- getChildren source
    files <- getKbyFiles source
    OsPath.withCurrentDirectory source $ mapM_ ( buildSite odir ) ( filter ( /= rootIndex ) ( files ++ children ) )
    end <- getCPUTime
    let time = fromIntegral (end-start) / (10^12)
      in printf "Finished in %0.4f sec.\n" (time :: Double)
--2}}}

buildSite :: Os.OsPath -> Os.OsPath -> IO () --{{{2
buildSite odir source = do
  OsPath.createDirectoryIfMissing True odir
  isFile <- OsPath.doesFileExist source
  case isFile of
    True -> buildPage odir source
    False -> do
      isDir <- OsPath.doesDirectoryExist source
      case isDir of
        False -> printf "[ERROR] %s is not a Kby file or directory." (show source)
        True -> do
          children <- getChildren source
          files <- getKbyFiles source
          OsPath.withCurrentDirectory source $ mapM_ ( buildSite ( Os.combine odir source ) ) ( files ++ children )
--2}}}

buildPage :: Os.OsPath -> Os.OsPath -> IO () --{{{2
buildPage outdir source = do
                 sourceString <- Os.decodeUtf source
                 outdirString <- Os.decodeUtf outdir >>= \x -> return $ x </> (dropExtension sourceString)
                 createDirectoryIfMissing True outdirString
                 let outpath = outdirString </> "index.html"
                 input <- readFile sourceString
                 printf "Building %s...\n" (dropFileName outpath)
                 case kbyToHtml sourceString (T.pack input) of
                   Left err -> do
                           let errType = case err of
                                           (LexError _) -> "lexical" :: String
                                           (ParseError _) -> "syntactic" :: String
                           printf "[ERROR] halting build of %s due to %s error:\n" (dropFileName outpath) errType
                           putStr $ unError err
                   Right page -> writeFile outpath page
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
    tomlRes <- Toml.decodeFileEither configCodec "kobayashi.toml"
    case tomlRes of
        Left errs      -> TIO.putStrLn $ Toml.prettyTomlDecodeErrors errs
        Right settings -> TIO.putStrLn $ Toml.encode configCodec settings
    processCMD opts cmd
--1}}}
