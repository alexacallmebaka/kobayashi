import System.IO
import Text.ParserCombinators.Parsec

markdownFile :: GenParser Char st [[String]]
markdownFile = do
  result <- many line
  eof
  return result

line :: GenParser Char st [String]
line = do
  first <- title <|> content
  rest <- remainingLines
  return (first:rest)

remainingLines :: GenParser Char st [String]
remainingLines = line <|> (return [])

title :: GenParser Char st String
title = string "# Hello, world!\n"

content :: GenParser Char st String
content = string "Mai best girl.\n"

main = do
  handle <- openFile "mai.md" ReadMode
  contents <- hGetContents handle
  print contents
  case parse markdownFile "(unknown)" contents of 
    Left e -> print e
    Right r -> mapM_ print r
  hClose handle
