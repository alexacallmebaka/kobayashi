--a module for HTML tyoeclass and related functions.
module HTML --{{{1
  ( Text(..)
  , DocuElem (..)
  , Tag (..)
  , TextStyle (..)
  , HTML(..)
  ) where
--1}}}

class HTML a where
    htmlify :: a -> String

--text. {{{1
data TextStyle = Bold | Italic deriving Show

data Text = RichText TextStyle [Text] | PlainText String deriving Show
--1}}}

--basic tagged elements. {{{1
data Tag = Header | Subheader | Paragraph deriving Show

data DocuElem = DocuElem Tag [Text] deriving Show
--1}}}

--helper functions. {{{1

--fold up a list of html elements into an HTML string.
htmlFold :: (HTML a) => [a] -> String --{{{2
htmlFold = foldl (\acc x -> acc ++ htmlify x) ""
--2}}}

--wrap a list of inner html in tags from outer html.
wrap :: (HTML a, HTML b) => a -> [b] -> String --{{{2
wrap outer inner = "<" ++ htmlify outer ++ ">" ++ htmlFold inner ++ "</" ++ htmlify outer ++ ">"
--2}}}

--1}}}

--define how things transform to html.

--basic tags. {{{1
instance HTML Tag where
    htmlify Header = "h1"  
    htmlify Subheader = "h2"  
    htmlify Paragraph = "p"  
--1}}}

--simple elements.
instance HTML DocuElem where
  htmlify (DocuElem tag text) = wrap tag text

--text styles. {{{1
instance HTML TextStyle where
    htmlify Bold = "strong"
    htmlify Italic = "em"
--1}}}

--text elements.
instance HTML Text where
    htmlify (PlainText x) = x
    htmlify (RichText style text)  = wrap style text
