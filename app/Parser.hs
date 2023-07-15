{-# LANGUAGE DeriveGeneric #-}

--a parser to parse a stream of tokens from the lexer.
module Parser (
        parseTokens
        ) where

import Text.Megaparsec
import Data.Void
import Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Hashable
import GHC.Generics (Generic)

import qualified KBYToken as KT
import qualified KBYDoc as KD

type Parser = Parsec Void KT.KBYStream

data InlineID = PlainText
                   | Bold
                   | Italic deriving (Eq, Generic)

instance Hashable InlineID

file :: Parser KD.Document
file = some blockElem

blockElem :: Parser KD.BlockElem
blockElem = choice [ oneTokenBlock KT.BeginHeader (\x -> KD.Header x)
                   , oneTokenBlock KT.BeginSubheader (\x -> KD.Subheader x)
                   , paragraph ]

paragraph :: Parser KD.BlockElem
paragraph = KD.Paragraph <$> some (inlineElem inlineChoices) <* endOfBlock

oneTokenBlock :: KT.KBYToken -> ([KD.InlineElem] -> KD.BlockElem) -> Parser KD.BlockElem
oneTokenBlock tok blockCon = do
        basicToken tok
        content <- many $ inlineElem inlineChoices
        endOfBlock
        return $ blockCon content
        
endOfBlock :: Parser ()
endOfBlock = (() <$ basicToken KT.EndOfBlock) <|> eof

--inline stuff {{{1
basicToken :: KT.KBYToken -> Parser KT.KBYToken
basicToken tok = token test Set.empty
    where test ( KT.KBYWithInfo _ _ t ) = if t == tok then Just t else Nothing

textChar :: Parser T.Text
textChar = token test Set.empty
    where test ( KT.KBYWithInfo _ c KT.TextChar) = Just c
          test _ = Nothing

plainText :: Parser KD.InlineElem
plainText = KD.PlainText . T.concat <$> some textChar

inlineChoices :: HM.HashMap InlineID (Parser KD.InlineElem)
inlineChoices = HM.fromList [ (Bold, wrapsText (\x -> KD.Bold x) KT.Bold Bold)
                          , (Italic, wrapsText (\x -> KD.Italic x) KT.Italic Italic)
                          , (PlainText, plainText) ]

inlineElem :: HM.HashMap InlineID (Parser KD.InlineElem) -> Parser KD.InlineElem
inlineElem elems = choice elems

wrapsText :: ([KD.InlineElem] -> KD.InlineElem) -> KT.KBYToken -> InlineID -> Parser KD.InlineElem
wrapsText wrapper tok obj = wrapper <$> between wrap wrap (some $ choice valid)
    where wrap = basicToken tok
          --need this because of how between operates underneath the hood.
          valid = HM.delete obj inlineChoices
 --1}}}
 --
parseTokens :: String -> KT.KBYStream -> Either (ParseErrorBundle KT.KBYStream Void) KD.Document
parseTokens = runParser file
