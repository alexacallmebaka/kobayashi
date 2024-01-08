{-# LANGUAGE DeriveGeneric #-}

--a module for HTML typeclass and related functions.

module HTML --{{{1
    ( HTML(..)
    , Tag(..)
    , wrap
    , htmlFold
    , genTags
    ) where
--1}}}

import Data.HashSet as HS
import Data.Hashable
import Data.Text hiding (foldl)
import GHC.Generics (Generic)

import Options

--types {{{1
--html tags

type LinkSource = Text

data Tag = Strong
         | Em
         | H1
         | H2
         | UL
         | LI
         | Pre
         | Code
         | A LinkSource
         | P deriving (Generic, Eq)

instance Hashable Tag

textify :: Tag -> Text
textify Strong = "strong"
textify Code = "code"
textify Em = "em"
textify H1 = "h1"
textify H2 = "h2"
textify UL = "ul"
textify LI = "li"
textify P = "p"
textify Pre = "pre"
textify (A _) = "a"
--1}}}

--tags that take up thier own line.
standaloneTags :: HS.HashSet Tag
standaloneTags = HS.fromList [P, UL, LI, Pre]

isStandalone :: Tag -> Bool
isStandalone x = HS.member x standaloneTags

class HTML a where
    htmlify :: Options -> a -> Text

--fold up a list of html elements into an HTML string.
htmlFold :: (HTML a) => Options -> [a] -> Text
htmlFold opts = foldl (\acc x -> acc `append` htmlify opts x) ""

--generate start and end tag strings from a tag.
genTags :: Tag -> (Text, Text)
genTags Pre  = ("<pre class=\"code\">\n", "</pre>")
genTags UL  = ("<ul>\n", "</ul>")
genTags LI  = ("<li>\n", "\n</li>\n")
genTags (A src) = (start, "</a>")
                where start = "<a href=\"" `append` src `append` "\">"
genTags x = ("<" `append` tag `append` ">" `append` nline, nline `append` "</" `append` tag `append` ">")
            where tag = textify x
                  --certain tags should be formatted on their own line.
                  nline = if isStandalone x then "\n"  else ""

--wrap a list of inner html in tags from outer html.
wrap :: (HTML a) => Options -> [a] -> Tag -> Text
wrap opts inner tag = open `append` (htmlFold opts inner) `append` close
               where (open, close) = genTags tag
