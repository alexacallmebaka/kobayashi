import System.IO
import Control.Applicative hiding ((<|>),many)
import Text.Parsec
import Web

--A file is a series of lines ended by eol.
file = endBy line eol

--A line is a title, subheader, or a text.
line = WebElem Header <$> header
  <|> WebElem SubHeader <$> subheader
  <|> WebElem Paragraph <$> (many text)

--Text is either bold, italic, or plain.
text = try boldText <|> try italicText <|> plainText

--The content of rich text is one or more of valid string characters.
richTextContent = many1 (noneOf "\n\r*")

--Plain text is just content.
plainText = (Text Plain) <$> richTextContent <?> "Plain Text"

--Bold text is rich text content wrapped in **.
boldText = (Text Bold) <$> (between (string "**") (string "**") richTextContent) <?> "Bold Text"

--Italic text is rich text content wrapped in *.
italicText = (Text Italic) <$> (between (string "*") (string "*") richTextContent) <?> "Bold Text"

--Header is some text preceded by # .
header = string "# " *> many text <?> "Title"

--Subheader is some text preceded by ^ .
subheader = string "^ " *> many text <?> "Subtitle"

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
    Right r -> putStr $ foldl (\acc x -> acc ++ htmlify x ++ "\n") "" r
  hClose handle
