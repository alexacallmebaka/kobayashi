--a module for HTML tyoeclass and related functions.
module HTML
  ( Html(..)
  ) where

import Document
import Data.List (intercalate)

--fold up a list of html elements into an HTML string.
htmlFold :: (Html a) => [a] -> String
--htmlFold = foldl (\acc x -> acc ++ wrap x) ""
htmlFold = foldl (\acc x -> acc ++ htmlify x) ""

class Html a where
    htmlify :: a -> String
--   wrap :: a -> String


--define how things transform to html.

--parser elements. {{{1
--instance Html Element where
      
--    htmlify (Header x) = "h1"  
--    htmlify (Subheader x) = "h2"  
--    htmlify (Paragraph x) = "p"  
--    htmlify (Bold x) = "strong"
--    htmlify (Italic x) = "em"
--    htmlify (PlainText x) = ""

--    wrap (PlainText x) = x
--    wrap (Paragraph x) = "<p>" ++ (htmlFold . intercalate [PlainText "\n"]) x ++ "<p>"
--    wrap a = "<" ++ htmlify a ++ ">" ++ htmlFold x ++ "<" ++ htmlify a ++ "/>"
--1}}} 

instance Html Element where
  htmlify (Header x) = "<h1>" ++ htmlFold x ++ "</h1>"
  htmlify (Subheader x) = "<h2>" ++ htmlFold x ++ "</h2>"
  htmlify (Paragraph x) = "<p>" ++ (htmlFold . intercalate [PlainText "\n"]) x ++ "</p>"

--parser text. {{{1
instance Html Text where
    htmlify (PlainText x) = x
    htmlify (Bold x) = "<strong>" ++ htmlFold x ++ "</strong>"
    htmlify (Italic x) = "<em>" ++ htmlFold x ++ "</em>"
