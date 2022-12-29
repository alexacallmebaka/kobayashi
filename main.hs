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
richText = try boldText <|> try italicText <|> plainText <?> "Rich Text"

--All metachars.
metaChars = "*\\"

--To escape a metachar, it is prefixed with a backslash.
escapedChar = char '\\' *> oneOf metaChars

--A Rich Text char is an escaped char or one valid non-metachar.
richTextChar = escapedChar <|> noneOf (metaChars ++ "\n\r") <?> "Character"

--Rich Text content is many rich text characters.
richTextContent = many1 richTextChar <?> "Rich Text Content"

--I think there is potnential for a left-recursive definition to do the nesting of bold and italics I want here. 
--Plaintext would act as a terminal.
--Try writing grammar to help figure this out?

--Plain text is just content.
plainText = (RichText Plain) <$> richTextContent <?> "Plain Text"

--Bold text is rich text content wrapped in **.
boldText = (RichText Bold) <$> (string "**" *> richTextContent <* string "**") <?> "Bold Text"

--Italic text is rich text content wrapped in *.
italicText = (RichText Italic) <$> (string "*" *> richTextContent <* string "*") <?> "Italic Text"

--Header is some text preceded by # .
header = string "# " *> many richText <?> "Title"

--Subheader is some text preceded by ## .
subheader = string "## " *> many richText <?> "Subtitle"

--End of line could be many things.
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
