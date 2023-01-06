module Parser
  ( parse
  , Element(..)
  ) where

import qualified Lexer as L
import Control.Applicative hiding ((<|>),many)
import Text.Parsec hiding (parse)

data Element = Header [Element] --{{{1
             | Subheader [Element] 
             | Paragraph [Element] 
             | Bold [Element] 
             | Italic [Element] 
             | PlainText String deriving Show
--1}}}

type Parser a = Parsec [L.Token] () a

--parse a simple token from the lexer.
basicTok :: L.Token -> Parser L.Token --{{{1
basicTok t = tokenPrim show nextPos testTok where
  nextPos pos x xs = incSourceColumn pos 1
  testTok x = if x == t then Just x else Nothing  
--1}}}

--parse a text token from the lexer.
textTok :: Parser String --{{{1
textTok = tokenPrim show nextPos testText where
  nextPos pos x xs = incSourceColumn pos 1
  testText (L.PlainText text) = Just text
  testText _  = Nothing  
--1}}}

--parser def {{{1
document = many element

element = header <|> subheader <|> paragraph <?> "document element"

header = basicTok L.Header *> (Header <$> (many1 text)) <?> "header"

subheader = basicTok L.Subheader *> (Subheader <$> (many1 text)) <?> "subheader"

paragraph = Paragraph <$> many1 text <?> "paragraph"

text = italicText <|> boldText <|> plainText <?> "text"

italicText = Italic <$> (basicTok L.Italic *> many1 (plainText <|> boldText) <* basicTok L.Italic) <?> "italic text"

boldText = Bold <$> (basicTok L.Bold *> many1 (plainText <|> italicText) <* basicTok L.Bold) <?> "bold text"

plainText = PlainText <$> textTok <?> "plain text"
--1}}}

parse :: SourceName -> [L.Token] -> Either ParseError [Element]
parse = runParser document ()
