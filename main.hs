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

data Element = Paragraph | Header | SubHeader deriving (Eq, Show)
data WebElem  =  WebElem {element :: Element, content :: Text} deriving (Eq, Show)

type Text = String
type Website = [WebElem]

file = endBy line eol

line = WebElem Header <$> title 
  <|> WebElem SubHeader <$> subtitle
  <|> WebElem Paragraph <$> text

--I think we define a paragraph a little differently. We go "many words" unless we find bold or italics which we can use 
-- <|> bold <|> italics <|> paragraph.

title = string "# " *> many (noneOf "\n\r") <?> "Title"
subtitle = string "^ " *> many (noneOf "\n\r") <?> "Subtitle"

text = many (noneOf "\n\r")

eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "EOL"

htmlify :: WebElem -> String
htmlify (WebElem Header text) = "<h1>" ++ text ++ "</h1>"
htmlify (WebElem SubHeader text) = "<h2>" ++ text ++ "</h2>"
htmlify (WebElem Paragraph text) = "<p>" ++ text ++ "</p>"

main = do
  handle <- openFile "index.mai" ReadMode
  contents <- hGetContents handle
  case parse file "(unknown)" contents of 
    Left e -> print e
    Right r -> mapM_ putStrLn $ map htmlify r
  hClose handle
