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
data WebElem  =  WebElem {element :: Element, content :: Text} deriving (Eq)
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
  <|> WebElem Paragraph <$> text


text = Text Bold <$> (try boldText) <|> Text Italic <$> (try italicText) <|> Text Plain <$> plainText <?> "Text"

plainText = many (noneOf "\n\r")
boldText = string "**" *> many (noneOf "*") <* string "**"
italicText = char '*' *> many (noneOf "*") <* char '*'

title = string "# " *> text <?> "Title"
subtitle = string "^ " *> text <?> "Subtitle"

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
