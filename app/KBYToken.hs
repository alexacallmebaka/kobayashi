--tokens output by the lexer.

--pragmas {{{1
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
--1}}}

--exports {{{1
module KBYToken
    ( KBYToken(..)
    , KBYWithInfo(..)
    , KBYStream(..)
    ) where
--1}}}

--imports {{{1
import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy(..))
import Data.List

import Text.Megaparsec
import Text.Megaparsec.Pos
import Text.Megaparsec.Stream
--1}}}

--types {{{1
data KBYToken = BeginHeader
              | BeginSubheader
              | UnorderedListItem
              | BeginImg
              | EndImg
              | BeginCodeListing
              | EndCodeListing
              | Bold 
              | Italic
              | Verb
              | LinkStart
              | LinkEnd
              | LinkSep
              | PageRef
              | AssetRef
              | EndOfBlock 
              | TextChar
              deriving (Eq, Show, Ord)

--a token with a starting position and text representation.
data KBYWithInfo = KBYWithInfo
    { startPos :: SourcePos
    , asTxt :: T.Text
    , innerToken :: KBYToken
    } deriving (Eq, Show, Ord)

--a token stream
newtype KBYStream = KBYStream { unStream :: [KBYWithInfo] } deriving (Eq, Show)
--1}}}

--megaparsec stream instances {{{1

--standard stream {{{2

--see: https://hackage.haskell.org/package/megaparsec-9.4.1/docs/Text-Megaparsec-Stream.html#t:Stream
--and: https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams

instance Stream KBYStream where 
    type Token KBYStream = KBYWithInfo
    type Tokens KBYStream = [KBYWithInfo]

    tokensToChunk Proxy = id
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    take1_ s = do
        taken <- uncons . unStream $ s
        rest <- return $ KBYStream . snd $ taken
        return (fst taken, rest)

    takeN_ _ (KBYStream []) = Nothing
    takeN_ x s 
        | x <= 0 = Just ([], s)
        | otherwise = do 
            let taken = splitAt x $ unStream s
            rest <- return $ KBYStream . snd $ taken
            return (fst taken, rest)

    takeWhile_ pred s = (fst pair, KBYStream . snd $ pair)
        where pair = span pred $ unStream s
--2}}}

--stream that can be printed {{{2
--see: https://hackage.haskell.org/package/megaparsec-9.4.1/docs/Text-Megaparsec-Stream.html#t:VisualStream

instance VisualStream KBYStream where
    showTokens Proxy = foldl (\acc x -> acc ++ (T.unpack . asTxt $ x)) ""
    tokensLength Proxy = foldl (\acc x -> acc + (T.length . asTxt $ x)) 0 
--2}}}

--stream with some useful stuff for error messages {{{2
--see: https://hackage.haskell.org/package/megaparsec-9.4.1/docs/Text-Megaparsec-Stream.html#t:TraversableStream

instance TraversableStream KBYStream where
    --this is not very well documented, so I am just trying something and seeing how it breaks lol
    reachOffset offset PosState {..} = ((Just line)
                                       , PosState
                                            { pstateInput = KBYStream post
                                            , pstateOffset = offset
                                            , pstateSourcePos = newSourcePos
                                            , pstateTabWidth = pstateTabWidth
                                            , pstateLinePrefix = pstateLinePrefix
                                            }
                                        )
                                        where
                                            tokenList = unStream pstateInput
                                            (pre, post) = splitAt offset tokenList
                                            newSourcePos = startPos . head $ post
                                            
                                            --get line number where error occurred.
                                            badLine = sourceLine newSourcePos

                                            --helper func to extract line number from KBYWithInfo
                                            getLine = sourceLine . startPos

                                            --get all tokens on the line with error.
                                            line = foldl (\acc x -> acc ++ (T.unpack . asTxt $ x)) "" stream
                                              where stream = [x | x <- tokenList, getLine x == badLine]
--2}}}

--1}}}
