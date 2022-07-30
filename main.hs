import System.IO
import Text.ParserCombinators.Parsec

--Brain dump:
--I can have all of the content for a website defined by different monads
--They are values with contexts (e.g. titles, footers, bold, etc.)
--I can have strings with the context of them being a title, or being bold, etc.
--Put all of the monads in a queue as I parse them, and then have a function that will pattern match on the
--Different monads and decide how to make them HTML. Since my website will be of a serial nature for most pages,
--I can just step through the queue.
--Will have a "Web element" typeclass.
--Also, I will definitely need to make my own markup.
--It may actually be better to just have a "webElem" monad, which will have the content, and then what the context is (i.e. what element it is).
--In fact, instead of a queue we could define a website type defined as a list of webElems. Then, it is a functor
--and do fun things with it. Might be able to then just fmap 'htmlify' function over the site to generate a page. Have the parser generate the website.
--htmlify would probably be something that takes a file handler as an arg too, so then im not constantly closing
--and reopening files. Since we would be doing IO in htmlify, it might end up being MapM as opposed to fmap.
--It could be useful to have as monads, but for now I think types are best.

--Need to implement a function that returns a GenParser for WebElem. Like the string function.
data WebElem  =  WebElem {context :: String, stuff :: String}
type Website = [WebElem]

markdownFile :: GenParser Char st Website
markdownFile = do
  result <- many line
  eof
  return result

line :: GenParser Char st WebElem
line = do
  result <- content
  eol
  return result

content :: GenParser Char st WebElem
content = do
  head <- title <|> text
  return head

title :: GenParser Char st WebElem
title = WebElem "Header" "# Hello, world!"

text :: GenParser Char st WebElem
text = WebElem "PlainText" "Mai best girl."

eol :: GenParser Char st Char
eol = char '\n'

main = do
  handle <- openFile "mai.md" ReadMode
  contents <- hGetContents handle
  case parse markdownFile "(unknown)" contents of 
    Left e -> print e
    Right r -> mapM_ print r
  hClose handle
