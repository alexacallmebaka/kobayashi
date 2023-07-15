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

data InlineObj = PlainText
                   | Bold
                   | Italic deriving (Eq, Generic)

instance Hashable InlineObj

basicInlineToken :: KT.KBYToken -> Parser KT.KBYToken
basicInlineToken tok = token test Set.empty
    where test ( KT.KBYWithInfo _ _ t ) = if t == tok then Just t else Nothing
          test _ = Nothing

textChar :: Parser T.Text
textChar = token test Set.empty
    where test ( KT.KBYWithInfo _ c KT.TextChar) = Just c
          test _ = Nothing

plainText :: Parser KD.InlineElem
plainText = KD.PlainText . T.concat <$> some textChar

inlineElems :: HM.HashMap InlineObj (Parser KD.InlineElem)
inlineElems = HM.fromList [ (Bold, wrapsText (\x -> KD.Bold x) KT.Bold Bold)
                          , (Italic, wrapsText (\x -> KD.Italic x) KT.Italic Italic)
                          , (PlainText, plainText) ]

inlineElem :: HM.HashMap InlineObj (Parser KD.InlineElem) -> Parser KD.InlineElem
inlineElem valid = choice valid

wrapsText :: ([KD.InlineElem] -> KD.InlineElem) -> KT.KBYToken -> InlineObj -> Parser KD.InlineElem
wrapsText wrapper tok obj = wrapper <$> between wrap wrap (many $ inlineElem valid)
    where wrap = basicInlineToken tok
          valid = HM.delete obj inlineElems
        
parseTokens :: String -> KT.KBYStream -> Either (ParseErrorBundle KT.KBYStream Void) [KD.InlineElem]
parseTokens = runParser (many $ inlineElem inlineElems)
