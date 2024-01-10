{-combines all modules of kobayashi for a full kby => html pipeline.
also handles creation of files and directories during build.
-}

--pragmas {{{1

--used to partially apply the MonadReader constraint to only types with "Options" as the environment type.
{-# LANGUAGE FlexibleContexts #-}

--used for pattern matching Path data constructor in Buildable typeclass.
{-# LANGUAGE FlexibleInstances #-}

--used for Buildable typeclass defintion
{-# LANGUAGE MultiParamTypeClasses #-}

--used for QuasiQuoters for Path module.
{-# LANGUAGE QuasiQuotes #-}

--1}}}

--exports {{{1
module Builder
    ( 
      Builder
    , build
    , runBuilder
    ) where
--1}}}

--imports {{{1
import Control.Monad (filterM, forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, MonadReader, ReaderT, runReaderT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (execWriterT, tell, WriterT)
import Data.Char (toLower)
import Data.Text (Text, pack, append)
import Path (Abs, Dir, File, Path, Rel, relfile, SomeBase (..), (</>))
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, listDirectory, withCurrentDirectory)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Printf (printf)

import Error (BuildError(..))
import Html (htmlify)
import Document (Document)
import Token (TokenStream)
import Lexer (lexFile)
import Options (Options(..))
import Parser (parseFile)

import qualified Path
import qualified Data.Text.IO as TIO
import qualified Data.Text as Text
import qualified System.FilePath as SysPath
--1}}}


--typeclass for valid sources for kobayashi to build from. {{{1
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

--1}}}

--Builder monad transformer stack and related functions. {{{1

{- Builder uses Writer to log any errors that occur during building, and 
reader to pass around options from the user.-}
type Builder = WriterT [BuildError] (ReaderT Options IO)

--pull IO monad out from Builder.
runBuilder :: Options -> Builder a -> IO [BuildError] --{{{2
runBuilder opts b = runReaderT ( execWriterT b ) opts
--2}}}

--1}}}

--IO helper functions {{{1

--get all children of a given directory.
getChildren :: Path b Dir -> IO [Path b Dir] --{{{2
getChildren dir = do
  --get contents of the given directory.
  dirContents <- listDirectory (Path.toFilePath dir)
  --get all directories in the given directory.
  children <- withCurrentDirectory (Path.toFilePath dir) (filterM doesDirectoryExist dirContents)
  --convert all of the children to Path Rel Dir types.
  typedChildren <- mapM Path.parseRelDir children
  --return the list of children, with their parent directory as a prefix.
  pure . map ( \x -> dir </> x ) $ typedChildren
--2}}}

--get all .kby files in a given directory (case-insensitive).
getKbyFiles :: Path b Dir -> IO [Path b File] --{{{2
getKbyFiles dir = do

  --get contents of given directory.
  dirContents <- listDirectory (Path.toFilePath dir)

  --get all files with .kby extension (case-insensitive).
  let kbyFiles = filter ( \x -> (map toLower . SysPath.takeExtension $ x) == ".kby" ) dirContents

  --type filenames as Path Rel File. 
  typedFileNames <- mapM Path.parseRelFile kbyFiles

  --return list of kby files with parent as prefix.
  pure $ map ( \x -> dir </> x ) typedFileNames
--2}}}

--given a path to a source kby file, generate the path to the corresponding html file in the build directory.
getFileDst :: Path b Dir -> Path Rel File -> IO (Path b File) --{{{2
getFileDst dst file 
  --if file is "index.kby" (case-insensitive), then just plop "index.html" in the build directory."
  | (map toLower . Path.toFilePath . Path.filename $ file) ==  "index.kby" = pure $ dst </> [relfile|index.html|]
  {-in all other cases, create a folder with the file's name in the build and place an "index.html" inside that folder.
  this has the affect of going to www.mysite.com/mySrc to view the html of mySrc.kby.
  -}
  | otherwise = (Path.parseRelDir . SysPath.dropExtension . Path.toFilePath $ file) >>= \x -> pure $ dst </> x </> [relfile|index.html|]
--2}}}

--1}}}

--functions to generate html-encoded text. {{{1

--comments to add some flair.
motd :: Text --{{{2
motd = "<!--Made with <3 using Kobayashi: https://github.com/alexacallmebaka/kobayashi-->\n"
--2}}}

--given a path, generate an html link to css.
css :: Path Abs File -> Text --{{{2
css path = "<link rel=\"stylesheet\" href=\"" `append` (pack . Path.toFilePath $ path) `append` "\">\n"
--2}}}

--convert the kbydoc ir to html-encoded text.
toHtml :: (MonadReader Options r) => Document -> r Text --{{{2
toHtml doc = do 
        opts <- ask 
        --for each block element in doc, turn it to html and append a newline. after that, combine list into one Text.
        content <- forM doc (\x -> htmlify x >>= pure . append "\n") >>= pure . Text.concat
        --combine everything for our final html page.
        pure $ "<!DOCTYPE HTML>\n" `append` motd `append` "<head>\n" `append` ( css $ oCssPath opts )
                  `append`  "</head>\n<html>\n<body>\n" `append`  content `append` "</body>\n</html>\n"
--2}}}

--convert kby-encoded text to html-encoded text.
kbyToHtml :: (MonadReader Options r) => String -> Text -> r (Either BuildError Text) --{{{2
kbyToHtml sourceFileName input = 
    case (lexFile sourceFileName input) >>= (parseFile sourceFileName) of
      Right doc -> toHtml doc >>= pure . Right
      Left x -> pure . Left $ x
--2}}}

--1}}}

--build directory and individual files. {{{1

--build a given directory.
buildFldr :: Path Rel Dir -> Path b Dir -> Builder () --{{{2
buildFldr buildDir src = do
  children <- liftIO . getChildren $ src
  files <- liftIO . getKbyFiles $ src
  --build each of the subdirectories, appending this directory to their buildDir.
  forM_ children (\x -> build (buildDir </> (Path.dirname x)) x)
  forM_ files (build buildDir)
--2}}}

--build a given kby file.
buildPage :: Path Rel Dir -> Path b File -> Builder () --{{{2
buildPage buildDir src = do
  --get options from environment.
  opts <- ask
  --get source path as string.
  let srcString = Path.toFilePath src
  input <- liftIO . TIO.readFile $ srcString
  liftIO . printf "Building %s...\n" $ srcString
  result <- lift . kbyToHtml srcString $ input
  case result of
    --if error, log error then exit.
    Left err -> tell [err]
    Right page -> do
          dst <- liftIO . getFileDst buildDir . Path.filename $ src
          --create build directory if it doesn't exist.
          liftIO . createDirectoryIfMissing True . Path.toFilePath . Path.parent $ dst
          liftIO . TIO.writeFile (Path.toFilePath dst) $ page
--2}}}
--1}}}
