module Main where

-- imports {{{1
import qualified Data.Map as Map

import Data.Text (Text, justifyLeft, justifyRight)
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

import Control.Monad.Writer (execWriterT)

import Builder
import Options
import Error
--1}}}

--actions {{{1

options :: [String] --{{{2
options = ["-odir", "-cfg"]
--2}}}

help :: IO () --{{{2
help = mapM_ TIO.putStr [ "Usage: kobayashi [options] <command> \n\n"
    ,"options\n"
    , "============\n"
    , justifyLeft 48 ' '"-odir /path/to/output/directory:"
    , justifyRight 50  ' ' "specify an output directory for the html files.\n"
    , justifyLeft 50 ' ' "-cfg /path/to/config.toml:"
    , justifyRight 50  ' ' "TOML file to use for project configuration details.\n"
    ,"\ncommands\n"
    , "============\n"
    , justifyLeft 50 ' ' "build /path/to/source/dir:"
    , justifyRight 50  ' ' "build a directory of .kby files (or a single .kby file) to html.\n"
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
                            errs <- if isKbyFile then 
                                         parseSomeFile arg >>= execWriterT . build opts (oBuildDir opts)
                                    else
                                         parseSomeDir arg >>= execWriterT . build opts (oBuildDir opts)
                            end <- getCPUTime
                            putChar '\n'
                            printErrors errs
                            let time = fromIntegral (end-start) / (10^12)
                                numErrs = length errs
                              in printf "Finished in %0.4f sec with %d error(s).\n" (time :: Double) numErrs
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
