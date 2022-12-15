import System.IO
import Control.Applicative hiding ((<|>),many)
import Text.Parsec
import Web

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
