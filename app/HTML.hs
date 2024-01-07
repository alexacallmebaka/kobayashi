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
import qualified Data.Text as T
import GHC.Generics (Generic)

import Options

--types {{{1
--html tags

type LinkSource = String

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

instance Show Tag where
    show Strong = "strong"
    show Code = "code"
    show Em = "em"
    show H1 = "h1"
    show H2 = "h2"
    show UL = "ul"
    show LI = "li"
    show P = "p"
    show Pre = "pre"
    show (A _) = "a"
--1}}}

--tags that take up thier own line.
standaloneTags :: HS.HashSet Tag
standaloneTags = HS.fromList [P, UL, LI, Pre]

isStandalone :: Tag -> Bool
isStandalone x = HS.member x standaloneTags

class HTML a where
    htmlify :: Options -> a -> String

--fold up a list of html elements into an HTML string.
htmlFold :: (HTML a) => Options -> [a] -> String 
htmlFold opts = foldl (\acc x -> acc ++ htmlify opts x) ""

--generate start and end tag strings from a tag.
genTags :: Tag -> (String, String)
genTags Pre  = ("<pre class=\"code\">\n", "</pre>")
genTags UL  = ("<ul>\n", "</ul>")
genTags LI  = ("<li>\n", "\n</li>\n")
genTags (A src) = (start, "</a>")
                where start = "<a href=\"" ++ src ++ "\">"
genTags x = ("<" ++ tag ++ ">" ++ nline, nline ++ "</" ++ tag ++ ">")
            where tag = show x
                  --certain tags should be formatted on their own line.
                  nline = if isStandalone x then "\n"  else ""

--wrap a list of inner html in tags from outer html.
wrap :: (HTML a) => Options -> [a] -> Tag -> String
wrap opts inner tag = open ++ (htmlFold opts inner) ++ close
               where (open, close) = genTags tag
