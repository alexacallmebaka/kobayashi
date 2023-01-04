import System.IO
import qualified Lexer as L
import Control.Applicative hiding ((<|>),many)
import Text.Parsec
import Control.Monad.Identity

data HTML = Bold [HTML] | Italic [HTML] | Header [HTML] | Subheader [HTML] | Paragraph [HTML] | PlainText String deriving Show

type Parser a = Parsec [L.Token] () a

--parse a simple token from the lexer.
basicTok :: L.Token -> Parser L.Token --{{{1
basicTok t = tokenPrim show nextPos testTok where
  nextPos pos x xs = incSourceColumn pos 1
  testTok x = if x == t then Just x else Nothing  
--1}}}

textTok :: Parser String --{{{1
textTok = tokenPrim show nextPos testText where
  nextPos pos x xs = incSourceColumn pos 1
  testText (L.PlainText text) = Just text
  testText _  = Nothing  
--1}}}

html = many element

element = header <|> text

header = basicTok L.Header *> (Header <$> (many1 text))

text = italicText <|> boldText <|> plainText

italicText = Italic <$> (basicTok L.Italic *> many1 (plainText <|> boldText) <* basicTok L.Italic)

boldText = Bold <$> (basicTok L.Bold *> many1 (plainText <|> italicText) <* basicTok L.Bold)

plainText = PlainText <$> textTok

parse1 :: String -> [HTML]
parse1 input = case L.tokenize "index.mai" input of
                Left e -> [PlainText $ show e]
                Right r -> case parse html "index.mai" r of
                             Left e -> [PlainText $ show e]
                             Right r -> r

main = do
  handle <- openFile "index.mai" ReadMode
  contents <- hGetContents handle
  print $ (parse1 contents)
  --Right r -> putStr $ foldl (\acc x -> acc ++ htmlify x ++ "\n") "" r
  hClose handle
