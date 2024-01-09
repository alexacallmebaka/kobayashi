{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

--build website from DocuElem.
module Builder
    ( build
    , Builder
    , runBuilder
    ) where

import Prelude hiding (concat)
import qualified Data.Text.IO as TIO
import Data.Text hiding (map, filter, concatMap, toLower)
import Text.Printf (printf)
import Data.Void
import Data.Char (toLower)
import Path
import qualified System.FilePath as SysPath
import System.Directory
import Control.Monad (filterM)
import Control.Monad.Writer
import Control.Monad.Reader

import Text.Megaparsec.Error

import HTML
import KBYDoc
import KBYToken
import Lexer
import Parser
import Options
import Error

--for Megaparsec
type SourceName = String

type Builder = WriterT [BuildError] (ReaderT Options IO)

runBuilder :: Options -> Builder a -> IO [BuildError]
runBuilder opts b = runReaderT ( execWriterT b ) opts

motd :: Text
motd = "<!--Made with <3 using Kobayashi: https://github.com/alexacallmebaka/kobayashi-->\n"

css :: Path Abs File -> Text
css path = "<link rel=\"stylesheet\" href=\"" `append` (pack . toFilePath $ path) `append` "\">\n"

--htmlify internal Document.
toHtml :: (MonadReader Options r) => Document -> r Text --{{{1
toHtml doc = do 
        opts <- ask 
        html <- mapM ( \x -> do 
          html <- htmlify x
          return $ html `append` "\n" ) doc
        content <- return $ concat html
        return $ "<!DOCTYPE HTML>\n" `append` motd `append` "<head>\n" `append` ( css $ oCssPath opts )
                  `append`  "</head>\n<html>\n<body>\n" `append`  content `append` "</body>\n</html>\n"
--1}}}

--lex a file and return a stream of tokens or an error as a string.
lexFile :: SourceName -> Text -> Either BuildError KBYStream
lexFile source input = case tokenize source input of
        Left err -> Left . LexError . pack $ errorBundlePretty err
        Right tokens -> Right tokens

--parse a file and return a stream of tokens or an error as a string.
parseFile :: SourceName -> KBYStream -> Either BuildError Document
parseFile source input = case parseTokens source input of
        Left err -> Left . ParseError . pack $ errorBundlePretty err
        Right doc -> Right doc

-- kby => html. {{{1
kbyToHtml :: (MonadReader Options r) => SourceName -> Text -> r (Either BuildError Text) --{{{2
kbyToHtml source input = 
    case (lexFile source input) >>= (parseFile source) of
      Right doc -> toHtml doc >>= return . Right
      Left x -> return . Left $ x
--2}}}

--1}}}

-- IO {{{1


class Buildable d s where
  build :: d -> s -> Builder ()

instance Buildable (Path Rel Dir) (SomeBase Dir) where
  build buildDir ( Abs dir ) = buildFldr buildDir dir
  build buildDir ( Rel dir ) = buildFldr buildDir dir

instance Buildable (Path Rel Dir) (SomeBase File) where
  build buildDir ( Abs file ) = buildPage buildDir file
  build buildDir ( Rel file ) = buildPage buildDir file

instance Buildable (Path Rel Dir) (Path b Dir) where
  build buildDir dir = buildFldr buildDir dir

instance Buildable (Path Rel Dir) (Path b File) where
  build buildDir file = buildPage buildDir file

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

getFileDst :: Path b Dir -> Path Rel File -> IO (Path b File) --{{{2
getFileDst dst file 
  | (map toLower . toFilePath . filename $ file) ==  "index.kby" = return $ dst </> [relfile|index.html|]
  | otherwise = (parseRelDir . SysPath.dropExtension . toFilePath $ file) >>= \x -> return $ dst </> x </> [relfile|index.html|]
--2}}}

--build a given directory.
buildFldr :: Path Rel Dir -> Path b Dir -> Builder () --{{{2
buildFldr buildDir src = do
  children <- liftIO . getChildren $ src
  files <- liftIO . getKbyFiles $ src
  mapM_ ( \x -> build ( buildDir </> (dirname x) ) x ) children
  mapM_ ( build buildDir ) files
--2}}}

--build a given kby file.
buildPage :: Path Rel Dir -> Path b File -> Builder () --{{{2
buildPage buildDir src = do
  opts <- ask
  let srcString = toFilePath src
  input <- liftIO . TIO.readFile $ srcString
  liftIO . printf "Building %s...\n" $ srcString
  result <- lift . kbyToHtml srcString $ input
  case result of
    Left err -> tell [err]
    Right page -> do
          (fileName,_) <- liftIO . splitExtension . filename $ src
          folderName <- liftIO . parseRelDir . toFilePath $ fileName
          dst <- liftIO . getFileDst buildDir . filename $ src
          liftIO . createDirectoryIfMissing True . toFilePath . parent $ dst
          liftIO . TIO.writeFile (toFilePath dst) $ page
--2}}}

--1}}}

--1}}}
