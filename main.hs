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
data WebElem  =  WebElem {element :: Element, content :: [[Text]]} deriving (Eq)
data Text = Text {style :: TextStyle, words :: String} deriving (Eq)
data TextStyle = Bold | Italic | Plain deriving (Eq)

instance Show Text where
     show (Text Bold text) = "<b>" ++ text ++ "</b>"
     show (Text Italic text) = "<i>" ++ text ++ "</i>"
     show (Text Plain text) = text

instance Show WebElem where
     show (WebElem Header text) = "<h1>" ++ (show text) ++ "</h1>"
     show (WebElem SubHeader text) = "<h2>" ++ (show text) ++ "</h2>"
     show (WebElem Paragraph text) = "<p>" ++ (show text) ++ "</p>"

--type Paragraph = [Text]
--type Website = [WebElem]

file = endBy line eol

line = WebElem Header <$> title 
  <|> WebElem SubHeader <$> subtitle
  <|> WebElem Paragraph <$> (many text)

text = try boldText <|> try italicText <|> plainText

word = many1 (noneOf "\n\r*")
richTextContent = sepBy1 word (char ' ')
plainText = map (Text Plain) <$> richTextContent <?> "Plain Text"
boldText = map (Text Bold) <$> (between (string "**") (string "**") richTextContent) <?> "Bold Text"
italicText = map (Text Italic) <$> (between (string "*") (string "*") richTextContent) <?> "Bold Text"

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
