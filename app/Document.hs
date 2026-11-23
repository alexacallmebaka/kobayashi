--kobayashi's intermediate representation.

--prgamas {{{1

--used to partially apply the MonadReader constraint to only types with "Options" as the environment type.
{-# LANGUAGE FlexibleContexts #-}

--1}}}

--exports {{{1
module Document
    (
      BlockElem(..)
    , Document(..)
    , Section(..)
    , InlineElem(..)
    , UnorderedListItem (..)
    , Url (..)
    , PhotoMetadata(..)
    ) where
--1}}}

--imports {{{1
import Data.Text (append, pack, Text, unpack)
import Control.Applicative ((<|>))
--1}}}

--types {{{1

data PhotoMetadata = PhotoMetadata 
  { iso :: Maybe Int
  , aperture :: Maybe Float
  , shutterSpeed :: Maybe Rational
  , cameraModel :: Maybe Text
  , focalLength :: Maybe Text
  } deriving (Eq, Show, Ord)

instance Semigroup PhotoMetadata where 
  lhs <> rhs = PhotoMetadata
        { iso = (iso lhs) <|> (iso rhs)
        , aperture = (aperture lhs) <|> (aperture rhs)
        , shutterSpeed = (shutterSpeed lhs) <|> (shutterSpeed rhs)
        , cameraModel = (cameraModel lhs) <|> (cameraModel rhs)
        , focalLength = (focalLength lhs) <|> (focalLength rhs)
        }

instance Monoid PhotoMetadata where
	mempty = PhotoMetadata Nothing Nothing Nothing Nothing Nothing

data Document = Document 
     { docTitle :: [InlineElem]
     , previewDesc :: Maybe Text
     , previewImagePath :: Maybe Url
     , docContents :: [Section] 
     }

data Section = Section
     { secTitle :: Maybe [InlineElem]
     , secContents :: [BlockElem]
     }

data Url = RemoteRef { refSrc :: Text }
         | PageRef { refSrc :: Text }
         | AssetRef { refSrc :: Text }
         deriving (Eq, Show, Ord)

data UnorderedListItem = UnorderedListItem [InlineElem] deriving (Eq, Show, Ord)

data BlockElem = Paragraph [InlineElem]
               | UnorderedList [UnorderedListItem]
               --1st list is quote, 2nd is author.
               | BlockQuote [InlineElem] [InlineElem]
               | Image Bool (Maybe PhotoMetadata) Url
               --label + list of elements.
               | Group Text [BlockElem]
               --Text is the verbatim code.
               | CodeListing Text
               deriving (Eq, Show, Ord)

--inline elements that represent rich text can be arbitrarily nested.
data InlineElem = Bold [InlineElem]
                | Italic [InlineElem]
                | Link [InlineElem] Url
                | Verb Text
                | PlainText Text 
                deriving (Eq, Show, Ord)
--1}}}

