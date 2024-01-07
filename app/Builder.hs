{-# LANGUAGE QuasiQuotes #-}

--build website from DocuElem.
module Builder
    ( build
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)
import Data.List
import Data.Void
import Path
import qualified System.FilePath as SysPath
import System.Directory
import Control.Monad (filterM)

import Text.Megaparsec.Error

import HTML
import KBYDoc
import KBYToken
import Lexer
import Parser
import Options
import Error

type SourceName = String

motd :: String
motd = "<!--Made with <3 using Kobayashi: https://github.com/alexacallmebaka/kobayashi-->\n"

css :: Path Abs File -> String
css path = "<link rel=\"stylesheet\" href=\"" ++ (toFilePath path) ++ "\">\n"

--htmlify internal Document.
toHTML :: Options -> Document -> String --{{{1
toHTML opts doc = "<!DOCTYPE HTML>\n" ++ motd ++ "<head>\n"++ ( css $ oCssPath opts ) ++ "</head>\n<html>\n<body>\n" ++  content ++ "</body>\n</html>\n"
    where content = concatMap ( (++ "\n") . (htmlify opts) ) doc
--1}}}

--lex a file and return a stream of tokens or an error as a string.
lexFile :: SourceName -> T.Text -> Either BuildError KBYStream
lexFile source input = case tokenize source input of
        Left err -> Left . LexError $ errorBundlePretty err
        Right tokens -> Right tokens

--parse a file and return a stream of tokens or an error as a string.
parseFile :: SourceName -> KBYStream -> Either BuildError Document
parseFile source input = case parseTokens source input of
        Left err -> Left . ParseError $ errorBundlePretty err
        Right doc -> Right doc


-- kby => html. {{{1
kbyToHtml :: Options -> SourceName -> T.Text -> Either BuildError String --{{{2
kbyToHtml opts source input = 
    case (lexFile source input) >>= (parseFile source) of
        Right doc -> Right $ toHTML opts doc
        Left x -> Left x
--2}}}

--1}}}

-- IO {{{1

getChildren :: Path Rel Dir -> IO [Path Rel Dir] --{{{2
getChildren dir = do
  dirContents <- listDirectory (toFilePath dir)
  children <- withCurrentDirectory (toFilePath dir) (filterM doesDirectoryExist dirContents)
  typedChildren <- mapM parseRelDir children
  return $ map ( \x -> dir </> x ) typedChildren
--2}}}

getKbyFiles :: Path Rel Dir -> IO [Path Rel File] --{{{2
getKbyFiles dir = do
  dirContents <- listDirectory (toFilePath dir)
  let kbyFiles = filter ( \x -> SysPath.takeExtension x == ".kby" || SysPath.takeExtension x == ".KBY" ) dirContents
  typedFileNames <- mapM parseRelFile kbyFiles
  return $ map ( \x -> dir </> x ) typedFileNames
--2}}}

--top level build. take special inital steps and the recursively builds
--the whole directory
--since we now treat standalone index as special i probably do not need to take a special index name, i think i can
--just call buildir now... maybe... No! Asset dir is special, but maybe i check for that everywhere because in theory
--it could be anywhere.
build :: Options -> Path Rel Dir -> IO () --{{{2
build opts src = do
    let srcString = toFilePath src
    let homepage = oHomepageName opts
    input <- TIO.readFile $ srcString SysPath.</> homepage
    printf "Building %s...\n" $ srcString SysPath.</> homepage
    case kbyToHtml opts homepage input of
      Left err -> do
        let errType = case err of
                       (LexError _) -> "lexical" :: String
                       (ParseError _) -> "syntactic" :: String
        --TODO: print to stderr, gather and report errors at some point??
        --maybe gather errors in list or something as we go along and print a summary of errors/not at end??
        --maybe i just return a list of unexecuted io actions and then execute everything with a sequebce
        --i can return either Err IO Text, then partition and report off of that?
        --maybe either Err (IO ())??
        --OR!! MAYBE I USE WRITERT/STATET TO DO IO AND MY LOGGING AT THE SAME TIME
        printf "[ERROR] halting build of %s due to %s error:\n" srcString errType
        putStr $ unError err
      Right page -> writeFile ( toFilePath $ (oBuildDir opts)</>[relfile|index.html|] ) page
    children <- getChildren src
    files <- getKbyFiles src
    typedHomepage <- parseRelFile homepage >>= \x -> return $ src </> x
    let assetsDirAbsStr = toFilePath $ oAssetsDir opts
    typedAssetsDirRel <- parseRelDir $ srcString SysPath.</> ( SysPath.makeRelative "/"  assetsDirAbsStr )
    mapM_ ( buildDir opts ) $ filter (/= typedAssetsDirRel) children
    mapM_ ( buildPage opts ) $ filter (/= typedHomepage) files
--2}}}

--build a given (non top level) directory.
buildDir :: Options -> Path Rel Dir -> IO () --{{{2
buildDir opts src = do
  children <- getChildren src
  files <- getKbyFiles src
  mapM_ ( buildDir opts ) children
  mapM_ ( buildPage opts ) files
--2}}}

--build a given kby file.
buildPage :: Options -> Path Rel File -> IO () --{{{2
buildPage opts src = do
  let srcString = toFilePath src
  (fileName,_) <- splitExtension $ filename src
  folderName <- parseRelDir $ toFilePath fileName
  --treat standalone index as special, can probably be optimized
  dir <- case ( (filename src) == [relfile|index.kby|] ||  (filename src) == [relfile|index.KBY|] ) of
           False -> replaceProperPrefix [reldir|src|] (oBuildDir opts) src >>= \x -> return $ parent x </> folderName
           True -> replaceProperPrefix [reldir|src|] (oBuildDir opts) src >>= \x -> return $ parent x
  createDirectoryIfMissing True (toFilePath dir)
  input <- TIO.readFile srcString
  printf "Building %s...\n" srcString
  case kbyToHtml opts srcString input of
    Left err -> do
      let errType = case err of
                      (LexError _) -> "lexical" :: String
                      (ParseError _) -> "syntactic" :: String
      printf "[ERROR] halting build of %s due to %s error:\n" srcString errType
      putStr $ unError err
    Right page -> writeFile ( toFilePath $ dir </> [relfile|index.html|] ) page
--2}}}

--1}}}

--1}}}
