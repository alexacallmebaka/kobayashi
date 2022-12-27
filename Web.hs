--This module exports relevant types for dealing with html elements.
module Web
( Element(..)
, WebElem(..)
, RichTextStyle(..)
, RichText(..)
, Html(..)
) where

import Data.Monoid
import Data.List

--A typeclass for html elements.
class Html a where
  htmlify :: a -> String

--Element defines the type of html tag.
data Element = Header | SubHeader | Paragraph

--RichTextStyle types define the styling of  piece of text (done through html tags if style is not plain).
data RichTextStyle = Bold | Italic | Plain

--A piece of RichText has a style and the string contents.
data RichText = RichText {style :: RichTextStyle, words :: String}

--WebElem types contains a type of html tag and the text contents of that tag.
data WebElem  =  WebElem {element :: Element, content :: [RichText]}

--Elements and RichTextStyles show the corresponding html tags.
instance Html Element where
  htmlify Header = "h1" 
  htmlify SubHeader = "h2"  
  htmlify Paragraph = "p" 

instance Html RichTextStyle where
  htmlify Bold = "b"
  htmlify Italic = "i"
  htmlify Plain = ""

--To turn text into html we wrap the text contents in the proper tags. 
instance Html RichText where
  --Plain text has no html tags.
  htmlify (RichText Plain text) = text
 --intercalate " " text
  htmlify (RichText style text) = 
    let open = "<" ++ htmlify style ++ ">"
        content = htmlify (RichText Plain text)
        close = "</" ++ htmlify style ++ ">"
    in open ++ content ++ close

--To turn a WebElem to html, we wrap the contents in the proper tags.
--Since the contents are a list of RichText with potentially different styles, 
--We turn each RichText to html and the concatenate them into one string.
instance Html WebElem where
  htmlify (WebElem elem text) =
    let open = "<" ++ htmlify elem ++ ">"
        content = foldl (\acc x -> acc ++ htmlify x) "" text 
        -- $ intersperse (RichText Plain [" "]) text
        close = "</" ++ htmlify elem ++ ">"
    in open ++ content ++ close
