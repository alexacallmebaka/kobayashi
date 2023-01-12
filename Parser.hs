--a parser to parse a stream of tokens from the lexer.
module Parser
  ( parse
  ) where

import qualified Lexer as L
import Control.Applicative hiding ((<|>),many,optional)
import Text.Parsec hiding (parse)
import Document



type Parser a = Parsec [L.TokenPos] () a

--base for making token parsers. {{{1
tokenParserBase :: (L.TokenPos -> Maybe a) -> Parser a
tokenParserBase = tokenPrim (show . fst) advance where
  --the advance function will grab the position of the next token if it exists. If it doesn't, it will return the last position.
  advance _ _ ((_, pos) : _) = pos
  advance pos _ [] = pos
--1}}}

--parse a simple token from the lexer.
basicTok :: L.Token -> Parser L.Token --{{{1
basicTok t = tokenParserBase testTok where
  testTok (x, y) = if x == t then Just x else Nothing  
--1}}}

--parse a text token from the lexer.
textTok :: Parser String --{{{1
textTok = tokenParserBase testText where
  testText (L.PlainText text, _) = Just text
  testText _  = Nothing  
--1}}}

--parser def {{{1
document = many element

element = header <|> subheader <|> paragraph <?> "document element"

--parse a header, and the text contents of the header.
header = basicTok L.Header *> (Header <$> (many1 text)) <* eol <?> "header"

--parse a subheader, and the text contents of the header.
subheader = basicTok L.Subheader *> (Subheader <$> (many1 text)) <* eol <?> "subheader"

--a bunch of lines makes a paragraph. A paragraph is ended by two newlines, another element, or eof.
paragraph = Paragraph <$> many1 line <* optional eol <?> "paragraph"

--a line is many text followed by an eol.
line = many1 text <* eol

--text can be bold, italic, or plain.
text = italicText <|> boldText <|> plainText <?> "text"

--italic text contains plain or bold parts.
italicText = Italic <$> (basicTok L.Italic *> many1 (plainText <|> boldText) <* basicTok L.Italic) <?> "italic text"

--bold text contains plain or italic parts.
boldText = Bold <$> (basicTok L.Bold *> many1 (plainText <|> italicText) <* basicTok L.Bold) <?> "bold text"

--plain text is just a plaintext token.
plainText = PlainText <$> textTok <?> "plain text"

eol = basicTok L.EOL
--1}}}

parse :: SourceName -> [L.TokenPos] -> Either ParseError [Element]
parse = runParser document ()
