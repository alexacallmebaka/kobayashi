--parses a stream of tokens on kobayshi's intermediate representation.
 
--pragmas {{{1
--used for making InlineIds hashable using ghc's generics.
{-# LANGUAGE DeriveGeneric #-}
--1}}}

--exports {{{1
module Parser 
        (
          parseTokens
        ) where
--1}}}

--imports {{{1
import Control.Applicative (optional, (<|>))
import Control.Monad.Combinators (between, choice, many, some)
import Data.Hashable
import Data.Text (pack, Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Text.Megaparsec (eof, errorBundlePretty, failure, manyTill, try, Parsec, runParser, token)

import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as Text

import Error (BuildError(..))
import Token (RichToken(..), Token(..), TokenStream)

import qualified Document as IR
--1}}}


--types {{{1

--type alias to make signatures nicer.
type Parser = Parsec Void TokenStream

--these types are keys used in a hashmap to identify parsers for different types of
--inline tokens. for details on why we derive generic, see here:
--https://hackage.haskell.org/package/hashable-1.4.2.0/docs/Data-Hashable.html#g:4
data InlineId = PlainId
              | BoldId
              | ItalicId
              | LinkId deriving (Eq, Generic)

--use ghc's generics to make the inlineId's hashable.
instance Hashable InlineId

--1}}}

--block stuff {{{1

--parse a nonempty file.
stream :: Parser IR.Document
stream = IR.Document <$> some blockElem <* eof

blockElem :: Parser IR.BlockElem
blockElem = choice [ paragraph
                   , image 
                   , oneTokenBlock BeginHeader (\x -> IR.Header x)
                   , oneTokenBlock BeginSubheader (\x -> IR.Subheader x)
                   , unorderedList
                   , blockQuote
                   , codeListing
                   , subdocument ]

paragraph :: Parser IR.BlockElem
paragraph = IR.Paragraph <$> some inlineElem <* endOfBlock

--TODO: check that is a valid image type!
image :: Parser IR.BlockElem
image = do
  basicToken BeginImg
  src <- linkSource
  basicToken EndImg
  endOfBlock
  pure . IR.Image $ src

blockQuote :: Parser IR.BlockElem
blockQuote = do
  basicToken BlockQuote
  quote <- some inlineElem
  basicToken BlockQuoteAuthor
  author <- some inlineElem
  endOfBlock
  pure . IR.BlockQuote quote $ author

codeListing :: Parser IR.BlockElem
codeListing = do
  basicToken BeginCodeListing
  text <- Text.concat <$> some textChar
  basicToken EndCodeListing
  endOfBlock
  pure . IR.CodeListing $ text

subdocument :: Parser IR.BlockElem
subdocument = do
  label <- beginSubdocLabel
  content <- manyTill blockElem (try $ endSubdocLabel label)
  pure . IR.Subdocument label $ content

beginSubdocLabel :: Parser Text
beginSubdocLabel = do
  basicToken BeginSubdocLabel 
  label <- Text.concat <$> some textChar
  basicToken EndSubdocLabel
  pure label

endSubdocLabel :: Text -> Parser ()
endSubdocLabel startLabel = do
  basicToken BeginSubdocLabel 
  endLabel <- Text.concat <$> some textChar
  basicToken EndSubdocLabel
  if startLabel == endLabel 
     then pure ()
     --TODO: better error messages
     else failure Nothing Set.empty

unorderedList :: Parser IR.BlockElem
unorderedList = IR.UnorderedList <$> (some unorderedListItem <* endOfBlock)

unorderedListItem :: Parser IR.UnorderedListItem
unorderedListItem = do
  basicToken UnorderedListItem
  contents <- many inlineElem
  pure . IR.UnorderedListItem $ contents

--generic parser for blocks that begin with a single token and are ended by an
--EndOfBlock token.

oneTokenBlock :: Token
              -- ^^ token to begin block
              -> ([IR.InlineElem] -> IR.BlockElem) 
              --  ^^ type constructor that takes a list of inlines and returns a block.
              -> Parser IR.BlockElem

oneTokenBlock tok blockCon = do
        --parse begin token
        basicToken tok

        --content is just a bunch of inlines (at least one)
        content <- some inlineElem

        --get block end
        endOfBlock

        pure . blockCon $ content

--the end of a block can either be an EndOfBlock token or EOF
endOfBlock :: Parser ()
endOfBlock = (() <$ basicToken EndOfBlock) <|> eof

--1}}}

--inline stuff {{{1

--these next two use Set.empty since there is nothing i want to pass to the error messages.
--see the docs on token: https://hackage.haskell.org/package/megaparsec-9.4.1/docs/Text-Megaparsec.html#v:token

basicToken :: Token -> Parser Token
basicToken tok = token test Set.empty
    where test ( RichToken _ _ t ) = if t == tok then Just t else Nothing

link :: Parser IR.InlineElem
link = do
    basicToken LinkStart
    title <- some $ inlineElem
    basicToken LinkSep
    src <- linkSource
    basicToken LinkEnd
    pure . IR.Link title $ src

--need to give better parse errors here
linkSource :: Parser (IR.Url)
linkSource = do
    maybeRefType <- optional (basicToken PageRef <|> basicToken AssetRef)
    url <- Text.concat <$> some textChar
    case maybeRefType of
      Nothing -> pure . IR.RemoteRef $ url
      Just (refType) -> case refType of
        AssetRef -> pure . IR.AssetRef $ url
        PageRef -> pure . IR.PageRef $ url

--parse a plaintext character
textChar :: Parser Text
textChar = token test Set.empty
    where test ( RichToken _ c TextChar) = Just c
          test _ = Nothing

verb :: Parser IR.InlineElem
verb = IR.Verb . Text.concat <$> between (basicToken Verb) (basicToken Verb) (some textChar)

plainText :: Parser IR.InlineElem
plainText = IR.PlainText . Text.concat <$> some textChar

--a hashmap is used here so that we can eliminate a certain
--parser after it has been chosen. we need to do this to match
--tokens properly. for example, we will see "*word* is bold" as one bold
--word as opposed to starting a bold, and then another bold nested inside
--containing " is bold".
basicInlineChoices :: HM.HashMap InlineId (Parser IR.InlineElem)
basicInlineChoices = HM.fromList [ (BoldId, wrapsText (\x -> IR.Bold x) Bold BoldId)
                            , (ItalicId, wrapsText (\x -> IR.Italic x) Italic ItalicId)
                            , (PlainId, plainText)
                            , (LinkId, link) ]

--simply choose an inline elem to parse from a given hashmap.
basicInlineElem :: HM.HashMap InlineId (Parser IR.InlineElem) -> Parser IR.InlineElem
basicInlineElem elems = choice elems

--generic parser for inline elements that are wrapped in the same starting and ending token.
--allows for arbitrary nesting on inline elements (e.g. a bold sentence with only some italic words.)
wrapsText :: ([IR.InlineElem] -> IR.InlineElem) 
--          ^^ type constructor for element that is the "wrapper" of the other elements.
--          e.g. in "*word*" IR.Bold is the wrapper.
          -> Token 
--          ^^the token to parse for the wrapper start and end.
          -> InlineId 
--          ^^the key in the hashmap that identifies the  parser for the Token passed.
          -> Parser IR.InlineElem

wrapsText wrapper tok obj = wrapper <$> between wrap wrap (some $ choice valid)
    where wrap = basicToken tok
          --delete current token from hashmap to avoid matching issues
          valid = HM.delete obj basicInlineChoices

inlineElem :: Parser IR.InlineElem
inlineElem = choice [basicInlineElem basicInlineChoices, link, verb]

 --1}}}

--function to run Parser
parseTokens :: String -> TokenStream -> Either BuildError IR.Document --{{{2
parseTokens source input = case runParser stream source input of
        Left err -> Left . ParseError . pack . errorBundlePretty $ err
        Right doc -> Right doc
--2}}}
