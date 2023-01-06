module HTML
  ( Html(..)
  , htmlFold
  ) where

import Parser (Element(..))

htmlFold :: [Element] -> String
htmlFold e = foldl (\acc x -> acc ++ htmlify x) "" e

class Html a where
    htmlify :: a -> String

instance Html Element where
  htmlify (Header x) = "<h1>" ++ htmlFold x ++ "</h1>"
  htmlify (Subheader x) = "<h2>" ++ htmlFold x ++ "</h2>"
  htmlify (Paragraph x) = "<p>" ++ htmlFold x ++ "</p>"
  htmlify (Bold x) = "<strong>" ++ htmlFold x ++ "</strong>"
  htmlify (Italic x) = "<em>" ++ htmlFold x ++ "</em>"
  htmlify (PlainText x) = x
