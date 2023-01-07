--module for HTML tyoeclass and related functions.
module HTML
  ( Html(..)
  , htmlFold
  ) where

import Parser (Element(..))

--fold up a list of pasrser elements into an HTML string.
htmlFold :: [Element] -> String
htmlFold e = foldl (\acc x -> acc ++ htmlify x) "" e

class Html a where
    htmlify :: a -> String

--define how things thransform into HTML.
instance Html Element where
  htmlify (Header x) = "<h1>" ++ htmlFold x ++ "</h1>"
  htmlify (Subheader x) = "<h2>" ++ htmlFold x ++ "</h2>"
  htmlify (Paragraph x) = "<p>" ++ htmlFold x ++ "</p>"
  htmlify (Bold x) = "<strong>" ++ htmlFold x ++ "</strong>"
  htmlify (Italic x) = "<em>" ++ htmlFold x ++ "</em>"
  htmlify (PlainText x) = x
