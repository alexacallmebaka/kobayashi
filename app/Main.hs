--main program entrypoint.
module Main
  (
   main
  ) where

-- imports {{{1
import Control.Monad.Writer (execWriterT)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Text (Text, justifyLeft, justifyRight)
import Path (parseSomeFile, parseSomeDir)
import Text.Printf (printf)
import Toml (prettyTomlDecodeErrors)
import System.Directory (doesFileExist)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import System.Exit (die)

import Builder (build, runBuilder)
import Error (printErrors)

import qualified Data.Map.Strict as Map (empty, findWithDefault, insert, Map)
import qualified Data.Text.IO as TIO (putStr, putStrLn)
import qualified System.FilePath as SysPath (takeExtension)

import qualified Options
--1}}}

--list of command line option strings.
options :: [String] --{{{1
options = ["-odir", "-cfg"]
--1}}}

--display help message, detailing all commands and options.
help :: IO () --{{{1
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
--1}}}

--return map of representing the options an their arguments, as well as the command that remains from the command line.
parseOptions :: Map.Map String String -> [String] -> (Map.Map String String, [String]) --{{{1

--if there is at least one option, check if it is valid.
parseOptions optMap x@(opt:arg:xs) = case opt `elem` options of
                                      --if it is not valid, return the map and command line.
                                      False -> (optMap, x)
                                      --if it is valid, add it to the map and then parse the rest of the command line.
                                      True -> parseOptions newOptMap xs
                                          where newOptMap = Map.insert opt arg optMap              
--in all other cases, just return the map and the list.
parseOptions optMap x = (optMap, x)
--1}}} 

--run the command passed to kobayashi via the command line.
runCommand :: Options.Options -> [String] -> IO () --{{{1
runCommand _  [] = putStrLn "No commands, nothing to do..."
runCommand _ ["help"] = help
runCommand opts ("build":arg:[]) = do
                            printf "Starting build of %s\n" arg
                            start <- getCPUTime
                            let isKbyFile = (map toLower $ SysPath.takeExtension arg) == ".kby"
                            --building is done using the Builder monad transformer stack, which incorporates Writer for errors and
                            --Reader to pass options. runBuilder will return the log from Writer, which records any build errors.
                            errs <- if isKbyFile 
                                      --if input is a .kby file, parse the string as a SomeFile type, and then build the file.
                                      then parseSomeFile arg >>= runBuilder opts . build (Options.oBuildDir opts)
                                      --otherwise, parse string as SomeDir type and build directory.
                                      else parseSomeDir arg >>= runBuilder opts . build (Options.oBuildDir opts)
                            end <- getCPUTime
                            --output newline for readability to separate build info logs from error messages.
                            putChar '\n'
                            printErrors errs
                            --convert time to seconds.
                            let time = fromIntegral (end-start) / (10^12)
                            let numErrs = length errs
                            printf "Finished in %0.4f sec with %d error(s).\n" (time :: Double) numErrs

--if we can't match the command, either the wrong amount of arguments was passed or it is not valid.
runCommand _ x = mapM_ putStr $ 
                  [ "[INPUT ERROR]\n"
                  , "Invalid command line: \""
                  --combine elements of command line into one string to print.
                  , intercalate " " x
                  ,"\".\n"
                  , "Probable causes are: options passed out of order, invalid command, or wrong number of arguments.\n"
                  , "Run \"kobayashi help\" to see valid usage.\n"
                  ]
--1}}}    

--program entrypoint.
main = do --{{{1
    args <- getArgs
    
    --parse options from command.
    let (optMap, cmd) = parseOptions Map.empty args
    let cmdOpts = Options.partialOptionsFromOptMap optMap
    
    --serach optMap for path to toml file. if none is found, use default path "kobayashi.toml".
    let tomlPathString = Map.findWithDefault "kobayashi.toml" "-cfg" optMap
    hasToml <- doesFileExist tomlPathString
    
    --print toml path if it exists, then build a partialOptions record.
    --otherwise alert the user that a toml file was not found. and return an empty partialOptions.
    --it is important to not that the toml not existing is not always an error. 
    --the user may not want to use a toml file, so the default "kobayashi.toml" would not exist and it is okay.  
    tomlOpts <- if hasToml 
                  then do
                    putStrLn $ "Using: " ++ tomlPathString ++ "\n"
                    Options.partialOptionsFromToml tomlPathString
                  else do 
                    putStrLn $ tomlPathString ++ " not found!\n"
                    pure . Right $ mempty

    case tomlOpts of
        --if the toml file had errors, print them and exit.
        Left errs -> do
          TIO.putStrLn "[INPUT ERROR]\nError(s) in kobayashi.toml:"
          TIO.putStrLn $ prettyTomlDecodeErrors errs

        Right tomlOpts -> do
          --combine partialOptions to create Options.
          let opts = Options.makeOptions $ Options.defaultPartialOptions <> tomlOpts <> cmdOpts

          case opts of
            Right options -> do
              print options
              --print newline to separate options from command logs for readability.
              putChar '\n'
              runCommand options cmd

            --if there was an error creating options, print it and exit.
            Left err -> do
              TIO.putStrLn "[INPUT ERROR]\nError(s) in configuration:"
              print err
--1}}}
