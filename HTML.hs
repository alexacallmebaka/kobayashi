--a module for HTML tyoeclass and related functions.
module HTML
  ( Html(..)
  ) where

import Parser (Element(..), Text(..))
import Data.List

--fold up a list of html elements into an HTML string.
htmlFold :: (Html a) => [a] -> String
htmlFold = foldl (\acc x -> acc ++ htmlify x) ""

class Html a where
    htmlify :: a -> String

--define how things transform to html.

--parser elements. {{{1
instance Html Element where
  htmlify (Header x) = "<h1>" ++ htmlFold x ++ "</h1>"
  htmlify (Subheader x) = "<h2>" ++ htmlFold x ++ "</h2>"
  htmlify (Paragraph x) = "<p>" ++ (htmlFold . intercalate [PlainText "\n"]) x ++ "</p>"
--1}}} 

--parser text. {{{1
instance Html Text where
  htmlify (PlainText x) = x
  htmlify (Bold x) = "<strong>" ++ htmlFold x ++ "</strong>"
  htmlify (Italic x) = "<em>" ++ htmlFold x ++ "</em>"
--1}}}
