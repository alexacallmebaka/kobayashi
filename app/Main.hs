module Main where

-- imports {{{1
import qualified Data.Map as Map

import Data.Text (Text)
import Data.Char (toLower)
import Data.Monoid

import qualified Toml

import System.Directory (doesFileExist)
import Data.List (intercalate)
import System.Environment (getArgs)
import System.CPUTime (getCPUTime)
import System.Exit (die)
import Text.Printf (printf)
import Path
import qualified  System.FilePath as SysPath
import qualified Data.Text.IO as TIO

import Builder
import Options
--1}}}

--general todos:
--switch IO ops to text

--actions {{{1

options :: [String] --{{{2
options = ["-odir", "-cfg"]
--2}}}

help :: IO () --{{{2
help = mapM_ putStrLn [ "Usage: kobayashi [options] <command> \n"
    ,"options"
    , "============"
    , "-odir /path/to/output/directory:\t specify an output directory for the html files."
    , "-cfg /path/to/config.toml:\t TOML file to use for project configuration details."
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
                            printf "Starting build of %s\n" arg
                            start <- getCPUTime
                            let isKbyFile = (map toLower $ SysPath.takeExtension arg) == ".kby"
                            if isKbyFile then 
                                         parseSomeFile arg >>= build opts (oBuildDir opts)
                            else
                                         parseSomeDir arg >>= build opts (oBuildDir opts)
                            end <- getCPUTime
                            putChar '\n'
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
    let tomlPathString = Map.findWithDefault "kobayashi.toml" "-cfg" flags
    hasToml <- doesFileExist tomlPathString
    if (hasToml) then
      (putStrLn $ "Using: " ++ tomlPathString ++ "\n")
    else
      (putStrLn $ tomlPathString ++ " not found!\n")
    tomlOpts <- case hasToml of
                  False -> return . Right $ mempty
                  True -> partialOptionsFromToml tomlPathString
    case tomlOpts of
        Left errs -> do
          putStrLn "[ERROR] Error(s) in kobayashi.toml:"
          TIO.putStrLn $ Toml.prettyTomlDecodeErrors errs
        Right tomlOpts -> do
          let opts = makeOptions $ defaultPartialOptions <> tomlOpts <> cmdOpts
          case opts of
            Right options -> print options >> putChar '\n' >> processCMD options cmd
            Left err -> do
              ( putStrLn "[ERROR] Error(s) in configuration:" ) >> print err
--1}}}
