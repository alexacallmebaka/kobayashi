--a module for HTML typeclass and related functions.

module HTML --{{{1
    ( HTML(..)
    , Tag(..)
    , wrap
    , htmlFold
    , genTags
    ) where
--1}}}

--types {{{1
--html tags
data Tag = Strong
         | Em
         | H1
         | H2
         | P

instance Show Tag where
    show Strong = "strong"
    show Em = "em"
    show H1 = "h1"
    show H2 = "h2"
    show P = "p"
--1}}}

class HTML a where
    htmlify :: a -> String

--fold up a list of html elements into an HTML string.
htmlFold :: (HTML a) => [a] -> String 
htmlFold = foldl (\acc x -> acc ++ htmlify x) ""

--generate start and end tag strings from a tag.
genTags :: Tag -> (String, String)
genTags x = ("<" ++ tag ++ ">", "</" ++ tag ++ ">")
            where tag = show x

--wrap a list of inner html in tags from outer html.
wrap :: (HTML a) => [a] -> Tag -> String
wrap inner tag = open ++ (htmlFold inner) ++ close
               where (open, close) = genTags tag
