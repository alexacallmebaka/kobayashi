import System.IO
import Control.Applicative hiding ((<|>),many)
import Text.Parsec
import Web

--A file is a series of lines ended by eol.
file = endBy line eol

--A line is a title, subheader, or a text.
line = WebElem Header <$> try header
  <|> WebElem SubHeader <$> try subheader
  <|> WebElem Paragraph <$> (many richText)

--Text is either bold, italic, or plain.
richText = many (char ' ') *> (try boldText <|> try italicText <|> plainText) <?> "Rich Text"

metaChars = "*\\"

escapedChar = char '\\' *> oneOf metaChars

richTextChar = escapedChar <|> noneOf (metaChars ++ " \n\r") <?> "Character"

--The content of rich text is one or more of valid string characters.
richTextWord = many1 richTextChar <?> "Word"

richTextWords = sepEndBy1 richTextWord (char ' ') <?> "Words"

--I think there is potnential for a left-recursive definition to do the nesting of bold and italics I want here. 
--Plaintext would act as a terminal.
--Try writing grammar to help figure this out?

--Plain text is just content.
plainText = (RichText Plain) <$> richTextWords <?> "Plain Text"

--Bold text is rich text content wrapped in **.
boldText = (RichText Bold) <$> (string "**" *> richTextWords <* string "**") <?> "Bold Text"

--Italic text is rich text content wrapped in *.
italicText = (RichText Italic) <$> (string "*" *> richTextWords <* string "*") <?> "Italic Text"

--Header is some text preceded by # .
header = string "# " *> many richText <?> "Title"

--Subheader is some text preceded by ^ .
subheader = string "## " *> many richText <?> "Subtitle"

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
