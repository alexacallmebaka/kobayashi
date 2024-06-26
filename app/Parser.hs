--parses a stream of tokens on kobayshi's intermediate representation.

--TODO: throw parse error on mismatched group.
 
--pragmas {{{1
--used for making InlineIds hashable using ghc's generics.
{-# LANGUAGE DeriveGeneric #-}
--1}}}

--exports {{{1
module Parser 
        (
          parseTokens,
          inlineElem,
          linkSource
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
              | VerbId
              | LinkId deriving (Eq, Generic)

--use ghc's generics to make the inlineId's hashable.
instance Hashable InlineId

--1}}}

--block stuff {{{1

--parse a nonempty file.
stream :: Parser IR.Document
stream = do
  basicToken BeginTitle
  title <- some inlineElem <* endOfBlock
  pvImPath <- optional (basicToken BeginPvImUrl *> linkSource <* endOfBlock)
  pvDesc <- optional (basicToken BeginPvDesc *> (Text.concat <$> some textChar) <* endOfBlock)
  anon <- many blockElem
  sections <- many section
  eof
  pure . IR.Document title pvDesc pvImPath $ (IR.Section Nothing anon):sections

section :: Parser IR.Section
section = do
  basicToken BeginSection
  title <- some inlineElem <* endOfBlock
  content <- many blockElem
  pure . IR.Section (Just title) $ content

blockElem :: Parser IR.BlockElem
blockElem = choice [ paragraph
                   , image 
                   , unorderedList
                   , blockQuote
                   , codeListing
                   , group ]

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

group :: Parser IR.BlockElem
group = do
  label <- beginGroupLabel
  content <- manyTill blockElem (try $ endGroupLabel label)
  pure . IR.Group label $ content

beginGroupLabel :: Parser Text
beginGroupLabel = do
  basicToken BeginGroupLabel 
  label <- Text.concat <$> some textCharNoSpace
  basicToken EndGroupLabel
  pure label

endGroupLabel :: Text -> Parser ()
endGroupLabel startLabel = do
  basicToken BeginGroupLabel 
  endLabel <- Text.concat <$> some textCharNoSpace
  basicToken EndGroupLabel
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

--TODO: links can technically contain either links which is weird and i should fix it.
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
    maybeAssetRef <- optional (basicToken AssetRef)
    url <- Text.concat <$> some textCharNoSpace
    case maybeAssetRef of
      Just AssetRef -> pure . IR.AssetRef $ url
      Nothing -> if ( any ( flip (Text.isPrefixOf) url ) $ ["/", "..", ".", "#"] )
                    then pure . IR.PageRef $ url
                    else pure . IR.RemoteRef $ url

--parse a plaintext character
textChar :: Parser Text
textChar = token test Set.empty
    where test ( RichToken _ c TextChar) = Just c
          test _ = Nothing

--match a text char that is not a space and is printable.
textCharNoSpace :: Parser Text
textCharNoSpace = token test Set.empty
    where 
          test ( RichToken _ " " TextChar) = Nothing
          test ( RichToken _ c TextChar) = Just c
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
                                 , (LinkId, link)
                                 , (VerbId, verb) ]

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
