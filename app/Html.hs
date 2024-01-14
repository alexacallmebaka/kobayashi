--a module for the Html typeclass and related functions.

--pragmas {{{1

--used to make Tag derive the Generic typeclass, so then it can have a Hashable instance.
{-# LANGUAGE DeriveGeneric #-}

--used to partially apply the MonadReader constraint to only types with "Options" as the environment type.
{-# LANGUAGE FlexibleContexts #-}

--1}}}

--exports {{{1
module Html 
    ( 
      Html(..)
    , Tag(..)

    , meta
    , motd
    , includeCss

    , genTags
    , htmlFold
    , wrap
    ) where
--1}}}

--imports {{{1
import Control.Monad (foldM)
import Control.Monad.Reader (ask, MonadReader)
import Data.Hashable (Hashable)
import Data.Text (Text, append, pack)
import GHC.Generics (Generic)
import Path (toFilePath)

import Options(Options(..))

import qualified Data.HashSet as HS
--1}}}

{-typeclass for things that can be turned into html-encoded text.
the MonadReader will hold enviorment information such as the assets directory to use when creating image tags. 
-}
class Html a where --{{{1
    htmlify :: (MonadReader Options r ) => a -> r Text
--1}}}

--html tag data type, related functions and instances {{{1
--html tag data type {{{2
data Tag = Strong
         | Em
         | H1
         | H2
         | UL
         | LI
         | Pre
         | Code
         | P 
         deriving (Generic, Eq)
--2}}}

--make Tag hashable using GHC's generics.
instance Hashable Tag

--turn a tag into text!
textify :: Tag -> Text --{{{2
textify Strong = "strong"
textify Code = "code"
textify Em = "em"
textify H1 = "h1"
textify H2 = "h2"
textify UL = "ul"
textify LI = "li"
textify P = "p"
textify Pre = "pre"
--2}}}

--set with tags that take up thier own line (i.e. are followed by newline).
standaloneTags :: HS.HashSet Tag --{{{2
standaloneTags = HS.fromList [P, UL, LI, Pre]
--2}}}

--check if tag is in standalone set.
isStandalone :: Tag -> Bool --{{{2
isStandalone x = HS.member x standaloneTags
--2}}}

--generate start and end tag text from a tag datatype.
genTags :: Tag -> (Text, Text) --{{{2
genTags Pre  = ("<pre class=\"code\">\n", "</pre>")
genTags UL  = ("<ul>\n", "</ul>")
genTags LI  = ("<li>\n", "\n</li>\n")
genTags x = ("<" `append` tag `append` ">" `append` nline, nline `append` "</" `append` tag `append` ">")
            where tag = textify x
                  --certain tags should be formatted on their own line.
                  nline = if isStandalone x then "\n"  else ""
--2}}}

--1}}}

--utility functions for creating html-encoded text. {{{1

--fold up a list of html elements into an HTML string.
htmlFold :: (Html a, MonadReader Options r) => [a] -> r Text --{{{2
--for the lambda: turn element to html and then append that text to the accumulator.
htmlFold = foldM (\acc x -> htmlify x >>= pure . append acc) ""
--2}}}

--wrap a list of inner html in tags from outer html.
wrap :: (Html a, MonadReader Options r) => [a] -> Tag -> r Text --{{{2
wrap inner tag = do
              let (open, close) = genTags tag   
              content <- (htmlFold inner) 
              pure $ open `append` content `append` close
--2}}}

--generate a tag to include css.
includeCss :: (MonadReader Options r) => r Text --{{{2
includeCss = do
  opts <- ask
  let cssPath = pack . toFilePath . oCssPath $ opts
  pure $ "<link rel=\"stylesheet\" href=\"" `append` cssPath `append` "\" />\n"
--2}}}

--comment to add some flair.
motd :: Text --{{{2
motd = "<!--Made with <3 using Kobayashi: https://github.com/alexacallmebaka/kobayashi-->\n"
--2}}}

--add various meta tags.
meta :: Text
--sets encoding, viewport.
meta = "<meta charset=\"UTF-8\" />\n<meta name=\"viewport\" content=\"width=device-width,initial-scale=1\" />\n"

--1}}}
