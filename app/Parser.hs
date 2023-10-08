{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

--parses a stream of tokens from the lexer to an internal document representation.

module Parser (
        parseTokens
        ) where

--imports {{{1
import Text.Megaparsec
import Data.Void
import Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Hashable
import GHC.Generics (Generic)

import qualified KBYToken as KT
import qualified KBYDoc as KD
--1}}}

--types {{{1

--type alias to make signatures nicer.
type Parser = Parsec Void KT.KBYStream

--these types are keys used in a hashmap to identify parsers for different types of
--inline tokens. for details on why we derive generic, see here:
--https://hackage.haskell.org/package/hashable-1.4.2.0/docs/Data-Hashable.html#g:4
data InlineID = PlainText
              | Bold
              | Italic 
              | Link deriving (Eq, Generic)

instance Hashable InlineID

--1}}}

--block stuff {{{1

--parse a nonempty file.
file :: Parser KD.Document
file = some blockElem <* eof

blockElem :: Parser KD.BlockElem
blockElem = choice [ oneTokenBlock KT.BeginHeader (\x -> KD.Header x)
                   , oneTokenBlock KT.BeginSubheader (\x -> KD.Subheader x)
                   , paragraph ]

paragraph :: Parser KD.BlockElem
paragraph = KD.Paragraph <$> some inlineElem <* endOfBlock

--generic parser for blocks that begin with a single token and are ended by an
--EndOfBlock token.

oneTokenBlock :: KT.KBYToken
--               ^^ token to begin block
              -> ([KD.InlineElem] -> KD.BlockElem) 
--              ^^ type constructor that takes a list of inlines and returns a block.
              -> Parser KD.BlockElem

oneTokenBlock tok blockCon = do
        --parse begin token
        basicToken tok

        --content is just a bunch of inlines (at least one)
        content <- some inlineElem

        --get block end
        endOfBlock

        return $ blockCon content

--the end of a block can either be an EndOfBlock token or EOF
endOfBlock :: Parser ()
endOfBlock = (() <$ basicToken KT.EndOfBlock) <|> eof

--1}}}

--inline stuff {{{1

--these next two use Set.empty since there is nothing i
--want to pass to the error messages.
--see the docs on token: https://hackage.haskell.org/package/megaparsec-9.4.1/docs/Text-Megaparsec.html#v:token

basicToken :: KT.KBYToken -> Parser KT.KBYToken
basicToken tok = token test Set.empty
    where test ( KT.KBYWithInfo _ _ t ) = if t == tok then Just t else Nothing

link :: Parser KD.InlineElem
link = do
    basicToken KT.LinkStart
    title <- some $ inlineElem
    basicToken KT.LinkSep
    src <- linkSource
    basicToken KT.LinkEnd
    return $ KD.Link title src

linkSource :: Parser KD.URL
linkSource = do
    (KD.PlainText url) <- plainText
    case ( T.isPrefixOf "./" url ) of
      True -> case (snd $ T.breakOnEnd "." url) of
                "kby" -> return $ KD.LocalRef KD.KBY url
                _ -> failure Nothing Set.empty
      False -> case (fst $ T.breakOn "://" url) of
                "http" -> return $ KD.RemoteRef KD.HTTP url
                "https" -> return $ KD.RemoteRef KD.HTTPS url
                _ -> failure Nothing Set.empty

--parse a plaintext character
textChar :: Parser T.Text
textChar = token test Set.empty
    where test ( KT.KBYWithInfo _ c KT.TextChar) = Just c
          test _ = Nothing

plainText :: Parser KD.InlineElem
plainText = KD.PlainText . T.concat <$> some textChar

--a hashmap is used here so that we can eliminate a certain
--parser after it has been chosen. we need to do this to match
--tokens properly. for example, we will see "*word* is bold" as one bold
--word as opposed to starting a bold, and then another bold nested inside
--containing " is bold".
basicInlineChoices :: HM.HashMap InlineID (Parser KD.InlineElem)
basicInlineChoices = HM.fromList [ (Bold, wrapsText (\x -> KD.Bold x) KT.Bold Bold)
                            , (Italic, wrapsText (\x -> KD.Italic x) KT.Italic Italic)
                            , (PlainText, plainText)
                            , (Link, link) ]

--simply choose an inline elem to parse from a given hashmap.
basicInlineElem :: HM.HashMap InlineID (Parser KD.InlineElem) -> Parser KD.InlineElem
basicInlineElem elems = choice elems

--generic parser for inline elements that are wrapped in the same starting and ending token.
--allows for arbitrary nesting on inline elements (e.g. a bold sentence with only some italic words.)
wrapsText :: ([KD.InlineElem] -> KD.InlineElem) 
--          ^^ type constructor for element that is the "wrapper" of the other elements.
--          e.g. in "*word*" KD.Bold is the wrapper.
          -> KT.KBYToken 
--          ^^the token to parse for the wrapper start and end.
          -> InlineID 
--          ^^the key in the hashmap that identifies the  parser for the KT.KBYToken passed.
          -> Parser KD.InlineElem

wrapsText wrapper tok obj = wrapper <$> between wrap wrap (some $ choice valid)
    where wrap = basicToken tok
          --delete current token from hashmap to avoid matching issues
          valid = HM.delete obj basicInlineChoices

inlineElem :: Parser KD.InlineElem
inlineElem = choice [basicInlineElem basicInlineChoices, link]

 --1}}}

parseTokens :: String -> KT.KBYStream -> Either (ParseErrorBundle KT.KBYStream Void) KD.Document
parseTokens = runParser file
