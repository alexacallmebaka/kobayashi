--This module exports relevant types for dealing with html elements.

module Web
( Element(..)
, WebElem(..)
, TextStyle(..)
, Text(..)
) where

data Element = Header | SubHeader | Paragraph deriving (Eq)
data WebElem  =  WebElem {element :: Element, content :: [Text]} deriving (Eq)

data TextStyle = Bold | Italic | Plain deriving (Eq)
data Text = Text {style :: TextStyle, words :: String} deriving (Eq)

instance Show Element where
  show Header = "h1" 
  show SubHeader = "h2"  
  show Paragraph = "p" 

instance Show TextStyle where
  show Bold = "b"
  show Italic = "i"
  show Plain = ""

instance Show Text where
  show (Text Plain text) = text
  show (Text style text) = "<" ++ show style ++ ">" ++ text ++ "</" ++ show style ++ ">" 

instance Show WebElem where
  show (WebElem elem text) = "<" ++ show elem ++ ">" ++ foldl (\acc x -> acc ++ (show x)) "" text ++ "</" ++ show elem ++ ">"  
