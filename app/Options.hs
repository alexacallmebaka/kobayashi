{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Options
  ( Options(..)
  , PartialOptions(..)
  , Config (..)
  , makeOptions
  , defaultPartialOptions
  , configCodec      
  , partialOptionsFromFlags
  , partialOptionsFromToml
  ) where

--implements the partial options monoid design pattern
--https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67

import Data.Text (Text, pack, unpack, append)
import qualified Data.Semigroup --avoid collisons with monoid functions, paricularly the Last newtype.
import Data.Monoid
import Path
import Toml (TomlCodec, (.=), TomlDecodeError)
import qualified Toml
import qualified Data.Map as Map
import qualified System.FilePath as SysPath (FilePath)

import Error

--used for modeling our toml file
data Config = Config { projectOptions :: PartialOptions }

configCodec :: TomlCodec Config
configCodec = Config <$> Toml.table partialOptionsCodec "project" .= projectOptions

--absolute paths will treat the build dir as root
data Options = Options
  { oBuildDir :: Path Rel Dir
  , oAssetsDir :: Path Abs Dir
  , oCssPath :: Path Abs File
  } deriving (Eq)

instance Show Options where
  show Options{..} = "[Current Configuration]\nBuild Directory: " ++ (show oBuildDir)
                     ++ "\nAssets Directory: " ++ (show oAssetsDir)
                     ++ "\nPath to CSS: " ++ (show oCssPath)

data PartialOptions = PartialOptions
  { poBuildDir :: Last (Path Rel Dir)
  , poAssetsDir :: Last (Path Abs Dir)
  , poCssPath :: Last (Path Abs File)
  } deriving (Show, Eq)

instance Semigroup PartialOptions where
  lhs <> rhs = PartialOptions 
    { poBuildDir = poBuildDir lhs <> poBuildDir rhs
    , poAssetsDir = poAssetsDir lhs <> poAssetsDir rhs
    , poCssPath = poCssPath lhs <> poCssPath rhs
    }

instance Monoid PartialOptions where
  mempty = PartialOptions mempty mempty mempty

textToAbsFile :: Text -> Either Text (Path Abs File)
textToAbsFile input = maybe ( Left $ "Invalid absolute file path: " `append` input ) Right ( parseAbsFile $ unpack input )

textToAbsDir :: Text -> Either Text (Path Abs Dir)
textToAbsDir input = maybe ( Left $ "Invalid absolute directory path: " `append` input ) Right ( parseAbsDir $ unpack input )

textToRelDir :: Text -> Either Text (Path Rel Dir)
textToRelDir input = maybe (Left $ "Invalid relative directory path: " `append` input ) Right ( parseRelDir $ unpack input )

pathToText :: Path b t -> Text
pathToText = pack . toFilePath

--parse PartialOptions from "projects" table in TOML file.
partialOptionsCodec :: TomlCodec PartialOptions
partialOptionsCodec = PartialOptions
  <$> Toml.last ( Toml.textBy pathToText textToRelDir ) "build_dir" .= poBuildDir
  <*> Toml.last ( Toml.textBy pathToText textToAbsDir ) "assets_dir" .= poAssetsDir
  <*> Toml.last ( Toml.textBy pathToText textToAbsFile ) "css_path" .= poCssPath

--create a PartialOptions object from a map of parsed flags. 
partialOptionsFromFlags :: Map.Map String String -> PartialOptions
partialOptionsFromFlags flags = mempty
  { poBuildDir = Last $ ( Map.lookup "-odir" flags >>= parseRelDir ) }

partialOptionsFromToml :: SysPath.FilePath -> IO (Either [TomlDecodeError] PartialOptions)
partialOptionsFromToml path = do
  tomlRes <- Toml.decodeFileEither configCodec path
  case tomlRes of
    Right (Config tomlOpts)  -> return . Right $ tomlOpts
    Left x -> return . Left $ x

lastToEither :: ErrorMsg -> Last a -> Either ErrorMsg a
lastToEither errMsg (Last x) = maybe (Left errMsg) Right x

makeOptions :: PartialOptions -> Either ErrorMsg Options
makeOptions PartialOptions {..} = do
  oBuildDir <- lastToEither "Missing build directory" poBuildDir
  oAssetsDir <- lastToEither "Missing assets directory" poAssetsDir
  oCssPath <- lastToEither "Missing path to Css." poCssPath
  return Options {..}

defaultPartialOptions :: PartialOptions
defaultPartialOptions = PartialOptions
  { poBuildDir = pure [reldir|build|]
  , poAssetsDir = pure [absdir|/assets|]
  , poCssPath = pure $ [absdir|/assets|]</>[relfile|style.css|]
  }
