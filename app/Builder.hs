{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--build website from DocuElem.
module Builder
    ( build
    ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf (printf)
import Data.List
import Data.Char (toLower)
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

getChildren :: Path b Dir -> IO [Path b Dir] --{{{2
getChildren dir = do
  dirContents <- listDirectory (toFilePath dir)
  children <- withCurrentDirectory (toFilePath dir) (filterM doesDirectoryExist dirContents)
  typedChildren <- mapM parseRelDir children
  return $ map ( \x -> dir </> x ) typedChildren
--2}}}

getKbyFiles :: Path b Dir -> IO [Path b File] --{{{2
getKbyFiles dir = do
  dirContents <- listDirectory (toFilePath dir)
  let kbyFiles = filter ( \x -> (map toLower $ SysPath.takeExtension x) == ".kby" ) dirContents
  typedFileNames <- mapM parseRelFile kbyFiles
  return $ map ( \x -> dir </> x ) typedFileNames
--2}}}


--maybe i make a "buildable" typeclass or something that i make the paths members of so that i can call just a "build" function
--that is polymorphic to dir or file if i am allowed to get rid of top level builds now

class Buildable d s where
  build :: Options -> d -> s -> IO () 

instance Buildable (Path Rel Dir) (SomeBase Dir) where
  build opts buildDir ( Abs dir ) = buildFldr opts buildDir dir
  build opts buildDir ( Rel dir ) = buildFldr opts buildDir dir

instance Buildable (Path Rel Dir) (SomeBase File) where
  build opts buildDir ( Abs file ) = buildPage opts buildDir file
  build opts buildDir ( Rel file ) = buildPage opts buildDir file

instance Buildable (Path Rel Dir) (Path b Dir) where
  build opts buildDir dir = buildFldr opts buildDir dir

instance Buildable (Path Rel Dir) (Path b File) where
  build opts buildDir file = buildPage opts buildDir file

--TODO: print to stderr, gather and report errors at some point??
--maybe gather errors in list or something as we go along and print a summary of errors/not at end??
--maybe i just return a list of unexecuted io actions and then execute everything with a sequebce
--i can return either Err IO Text, then partition and report off of that?
--maybe either Err (IO ())??

getFileDst :: Path b Dir -> Path Rel File -> IO (Path b File) --{{{2
getFileDst dst file 
  | (map toLower . toFilePath . filename $ file) ==  "index.kby" = return $ dst </> [relfile|index.html|]
  | otherwise = (parseRelDir . SysPath.dropExtension . toFilePath $ file) >>= \x -> return $ dst </> x </> [relfile|index.html|]
--2}}}

--build a given directory.
buildFldr :: Options -> Path Rel Dir -> Path b Dir -> IO () --{{{2
buildFldr opts buildDir src = do
  children <- getChildren src
  files <- getKbyFiles src
  mapM_ ( \x -> build opts ( buildDir </> (dirname x) ) x ) children
  mapM_ ( build opts buildDir ) files
--2}}}

--build a given kby file.
buildPage :: Options -> Path Rel Dir -> Path b File -> IO () --{{{2
buildPage opts buildDir src = do
  let srcString = toFilePath src
  (fileName,_) <- splitExtension $ filename src
  folderName <- parseRelDir $ toFilePath fileName
  dst <- getFileDst buildDir (filename src)
  createDirectoryIfMissing True (toFilePath $ parent dst)
  input <- TIO.readFile srcString
  printf "Building %s...\n" srcString
  case kbyToHtml opts srcString input of
    Left err -> do
      let errType = case err of
                      (LexError _) -> "lexical" :: String
                      (ParseError _) -> "syntactic" :: String
      printf "[ERROR] halting build of %s due to %s error:\n" srcString errType
      putStr $ unError err
    --Right page -> writeFile ( toFilePath $ dir </> [relfile|index.html|] ) page
    Right page -> writeFile (toFilePath dst) page
--2}}}

--1}}}

--1}}}
