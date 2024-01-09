{- implements the partial options monoid design pattern for user-configurable project-wide options.
read more about this pattern: https://medium.com/@jonathangfischoff/the-partial-options-monoid-pattern-31914a71fc67
-}

--pragmas {{{1
{-# LANGUAGE RecordWildCards #-}

--so we can use quasiquoters from Path module
{-# LANGUAGE QuasiQuotes #-}
--1}}}

--exports {{{1
module Options
  ( 
    Config (..)
  , Options(..)
  , PartialOptions(..)
    
  , configCodec      
  , defaultPartialOptions
  , makeOptions
  , partialOptionsFromOptMap
  , partialOptionsFromToml
  ) where
--1}}}

--imports {{{1
import Data.Text (Text, pack, unpack, append)
import Path (absdir, Abs, Dir, File,  Path, Rel, reldir, relfile, (</>))
import Toml (TomlCodec, TomlDecodeError, (.=))

import Error (ErrorMsg)

import qualified Data.Map as Map
import qualified Data.Monoid as Monoid
import qualified Path
import qualified System.FilePath as SysPath
import qualified Toml
--1}}}

--Options and PartialOptions data types and typeclass instances. {{{1 

--record for project configuration options. absolute paths will treat the build directory as root.
data Options = Options --{{{2
  { oBuildDir :: Path Rel Dir
  , oAssetsDir :: Path Abs Dir
  , oCssPath :: Path Abs File
  } deriving (Eq)
--2}}}

--pretty print options.
instance Show Options where --{{{2
  show Options{..} = "[Current Configuration]\nBuild Directory: " ++ (show oBuildDir)
                     ++ "\nAssets Directory: " ++ (show oAssetsDir)
                     ++ "\nPath to CSS: " ++ (show oCssPath)
--2}}}

{- monoid used to build Options. 
the Monoid.Last newtype wraps a Maybe `value and will return the last
non-Nothing value. i.e. Last (Just x) <> Last Nothing == Last (Just x)
-}
data PartialOptions = PartialOptions --{{{2
  { poBuildDir :: Monoid.Last (Path Rel Dir)
  , poAssetsDir :: Monoid.Last (Path Abs Dir)
  , poCssPath :: Monoid.Last (Path Abs File)
  } deriving (Show, Eq)
--2}}}

{- every monoid instance must first define a Semigroup instance.
since all members of PartialOptions are Last which is already a monoid
we can just use the underlying <>.
-}
instance Semigroup PartialOptions where --{{{2
  lhs <> rhs = PartialOptions 
    { poBuildDir = poBuildDir lhs <> poBuildDir rhs
    , poAssetsDir = poAssetsDir lhs <> poAssetsDir rhs
    , poCssPath = poCssPath lhs <> poCssPath rhs
    }
--2}}}

--for the Monoid instance is trivial since each field is already a monoid
instance Monoid PartialOptions where --{{{2
  mempty = PartialOptions mempty mempty mempty
--2}}}

--1}}}
  
{- datatypes and codecs used by the tomland library to parse toml {{{1
see: https://hackage.haskell.org/package/tomland-1.3.3.2
-}

--datatype used to model the "[project]" table from toml file.
data Config = Config { projectOptions :: PartialOptions }

--codec to parse "[project]" table in toml file.
configCodec :: TomlCodec Config --{{{2
configCodec = Config <$> Toml.table partialOptionsCodec "project" .= projectOptions
--2}}}

--codec for creating PartialOptions from contents of "[project]" table in TOML file.
partialOptionsCodec :: TomlCodec PartialOptions --{{{2
partialOptionsCodec = PartialOptions
  <$> Toml.last ( Toml.textBy pathToText textToRelDir ) "build_dir" .= poBuildDir
  <*> Toml.last ( Toml.textBy pathToText textToAbsDir ) "assets_dir" .= poAssetsDir
  <*> Toml.last ( Toml.textBy pathToText textToAbsFile ) "css_path" .= poCssPath
--2}}}

--1}}}

--utility functions {{{1

{- utility functions that convert Text into Path types. {{{2
all of the Path.parse* functions return values wrapped in a member of
MonadThrow (https://hackage.haskell.org/package/exceptions-0.10.7/docs/Control-Monad-Catch.html#t:MonadThrow),
throwing errors if the parsed path is invalid (https://hackage.haskell.org/package/path-0.9.5/docs/Path-Posix.html#g:5).

here, we use Maybe as the ambient monad, which return a Just if successful and a Nothing if there is an error.
using the "maybe" function we sugar this up a bit and return an Either to make Toml.textBy happy, as it requires
an Either Text (https://hackage.haskell.org/package/tomland-1.3.3.2/docs/Toml-Codec-Combinator-Custom.html#v:textBy).
-}

textToAbsFile :: Text -> Either Text (Path Abs File) --{{{3
textToAbsFile input = maybe ( Left $ "Invalid absolute file path: " `append` input ) Right ( Path.parseAbsFile $ unpack input )
--3}}}

textToAbsDir :: Text -> Either Text (Path Abs Dir) --{{{3
textToAbsDir input = maybe ( Left $ "Invalid absolute directory path: " `append` input ) Right ( Path.parseAbsDir $ unpack input )
--3}}}

textToRelDir :: Text -> Either Text (Path Rel Dir) --{{{3
textToRelDir input = maybe (Left $ "Invalid relative directory path: " `append` input ) Right ( Path.parseRelDir $ unpack input )
--3}}}

--2}}}

pathToText :: Path b t -> Text --{{{2
pathToText = pack . Path.toFilePath
--2}}}

--utility function to covert Last value to an Either with a given error message.
lastToEither :: ErrorMsg -> Monoid.Last a -> Either ErrorMsg a --{{{2
lastToEither errMsg (Monoid.Last x) = maybe (Left errMsg) Right x
--2}}}

--1}}}

--Options and PartialOptions creation functions. {{{1

{- take an empty PartialOptions and replace the poBuildDir field 
with the "-odir" option in the given map. since lookup can return Nothing,
if "-odir" is not present than the field collapses to Monoid.Last Nothing.
otherwise, parse as a relative dir.
-}
partialOptionsFromOptMap :: Map.Map String String -> PartialOptions --{{{2
partialOptionsFromOptMap optMap = mempty
  { poBuildDir = Monoid.Last $ Map.lookup "-odir" optMap >>= Path.parseRelDir }
--2}}}

partialOptionsFromToml :: SysPath.FilePath -> IO (Either [TomlDecodeError] PartialOptions) --{{{2
partialOptionsFromToml path = do 

  --attempt to decode "[projects]" table in toml file.
  tomlRes <- Toml.decodeFileEither configCodec path
  
  --return the result wrapped in an either, pulling the partialOptions out of the Config.
  pure $ case tomlRes of
           Right (Config tomlOpts)  -> Right tomlOpts
           Left x -> Left x
--2}}}

--convert a given partialOptions to an Options, returning an error if any field remains empty.
makeOptions :: PartialOptions -> Either ErrorMsg Options --{{{2
makeOptions PartialOptions {..} = do
  oBuildDir <- lastToEither "Missing build directory" poBuildDir
  oAssetsDir <- lastToEither "Missing assets directory" poAssetsDir
  oCssPath <- lastToEither "Missing path to Css." poCssPath
  return Options {..}
--2}}}

defaultPartialOptions :: PartialOptions --{{{2
defaultPartialOptions = PartialOptions
  { poBuildDir = pure [reldir|build|]
  , poAssetsDir = pure [absdir|/assets|]
  , poCssPath = pure $ [absdir|/assets|]</>[relfile|style.css|]
  }
--2}}}

--1}}}
