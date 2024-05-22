--tokens output by the lexer.

--pragmas {{{1
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
--1}}}

--exports {{{1
module Token
    ( 
      RichToken(..)
    , Token(..)
    , TokenStream(..)
    ) where
--1}}}

--imports {{{1
import Data.List (uncons)
import Data.Text (Text, unpack)
import Data.Proxy (Proxy(..))
import Text.Megaparsec (PosState(..), Stream, TraversableStream, VisualStream)
import Text.Megaparsec.Pos (SourcePos(..))

import qualified Data.Text as Text
import qualified Text.Megaparsec.Stream
--1}}}


--types {{{1
data Token = BeginTitle
           | BeginSection
           | UnorderedListItem
           | BlockQuote
           | BlockQuoteAuthor
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
           | AssetRef
           | BeginGroupLabel
           | EndGroupLabel
           | EndOfBlock 
           | TextChar
           | BeginPvDesc
           | BeginPvImPath
           deriving (Eq, Show, Ord)

--a token with a starting position and text representation.
data RichToken = RichToken
    { startPos :: SourcePos
    , asTxt :: Text
    , innerToken :: Token
    } deriving (Eq, Show, Ord)

--a token stream
newtype TokenStream = TokenStream { unStream :: [RichToken] } deriving (Eq, Show)
--1}}}

--megaparsec stream instances {{{1

--standard stream {{{2

--see: https://hackage.haskell.org/package/megaparsec-9.4.1/docs/Text-Megaparsec-Stream.html#t:Stream
--and: https://markkarpov.com/tutorial/megaparsec.html#working-with-custom-input-streams

instance Stream TokenStream where 
    type Token TokenStream = RichToken
    type Tokens TokenStream = [RichToken]

    tokensToChunk Proxy = id
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    take1_ s = do
        taken <- uncons . unStream $ s
        rest <- pure $ TokenStream . snd $ taken
        pure (fst taken, rest)

    takeN_ _ (TokenStream []) = Nothing
    takeN_ x s 
        | x <= 0 = Just ([], s)
        | otherwise = do 
            let taken = splitAt x $ unStream s
            rest <- pure $ TokenStream . snd $ taken
            pure (fst taken, rest)

    takeWhile_ pred s = (fst pair, TokenStream . snd $ pair)
        where pair = span pred $ unStream s
--2}}}

--stream that can be printed {{{2
--see: https://hackage.haskell.org/package/megaparsec-9.4.1/docs/Text-Megaparsec-Stream.html#t:VisualStream

instance VisualStream TokenStream where
    showTokens Proxy = foldl (\acc x -> acc ++ (unpack . asTxt $ x)) ""
    tokensLength Proxy = foldl (\acc x -> acc + (Text.length . asTxt $ x)) 0 
--2}}}

--stream with some useful stuff for error messages {{{2
--see: https://hackage.haskell.org/package/megaparsec-9.4.1/docs/Text-Megaparsec-Stream.html#t:TraversableStream

instance TraversableStream TokenStream where
    --this is not very well documented, so I am just trying something and seeing how it breaks lol
    reachOffset offset PosState {..} = ((Just line)
                                       , PosState
                                         { pstateInput = TokenStream post
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

                                            --helper func to extract line number from RichToken
                                            getLine = sourceLine . startPos

                                            --get all tokens on the line with error.
                                            line = foldl (\acc x -> acc ++ (unpack . asTxt $ x)) "" stream
                                              where stream = [x | x <- tokenList, getLine x == badLine]
--2}}}

--1}}}
