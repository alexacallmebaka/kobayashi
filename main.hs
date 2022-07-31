import System.IO
import Control.Applicative hiding ((<|>),many)
import Text.Parsec

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

data WebElem  =  WebElem {context :: String, stuff :: String} deriving (Show, Eq)
type Website = [WebElem]

file = endBy line eol

line = WebElem "title" <$> title 
<|> WebElem "text" <$> text

title = string "# " *> many (noneOf "\n\r") <?> "Title"

text = many (noneOf "\n\r")

eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "EOL"

main = do
  handle <- openFile "index.mai" ReadMode
  contents <- hGetContents handle
  case parse file "(unknown)" contents of 
    Left e -> print e
    Right r -> mapM_ print r
  hClose handle
