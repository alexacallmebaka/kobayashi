import System.IO
import Control.Applicative hiding ((<|>),many)
import Text.Parsec

--TODO:
--Image
--link
--embedded html
--code (block and inline)
--templating
--bold, italics
--footnotes?


--TEXT SHOULD BE A MONAD???

data Element = Header | SubHeader | Paragraph deriving (Eq)
data WebElem  =  WebElem {element :: Element, content :: [Text]} deriving (Eq)
data Text = Text {style :: TextStyle, words :: String} deriving (Eq)
data TextStyle = Bold | Italic | Plain deriving (Eq)

instance Show Text where
     show (Text Bold text) = "<b>" ++ text ++ "</b>"
     show (Text Italic text) = "<i>" ++ text ++ "</i>"
     show (Text Plain text) = text

instance Show WebElem where
     show (WebElem Header text) = htmlify text "<h1>" "</h1>"
     show (WebElem SubHeader text) = htmlify text "<h2>" "</h2>"
     show (WebElem Paragraph text) = htmlify text "<p>" "</p>"

--I would like this to work with styling text too, the problem is that that will just be type Text not [Text] I think.
htmlify :: [Text] -> String -> String -> String
htmlify text open close = open ++ (foldl (\acc x -> acc ++ (show x)) "" text) ++ close

--type Paragraph = [Text]
--type Website = [WebElem]

file = endBy line eol

line = WebElem Header <$> title 
  <|> WebElem SubHeader <$> subtitle
  <|> WebElem Paragraph <$> (many text)

text = try boldText <|> try italicText <|> plainText

richTextContent = many1 (noneOf "\n\r*")
plainText = (Text Plain) <$> richTextContent <?> "Plain Text"
boldText = (Text Bold) <$> (between (string "**") (string "**") richTextContent) <?> "Bold Text"
italicText = (Text Italic) <$> (between (string "*") (string "*") richTextContent) <?> "Bold Text"

title = string "# " *> many text <?> "Title"
subtitle = string "^ " *> many text <?> "Subtitle"

eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "EOL"

main = do
  handle <- openFile "index.mai" ReadMode
  contents <- hGetContents handle
  case parse file "index.mai" contents of 
    Left e -> print e
    Right r -> mapM_ print r
  hClose handle
