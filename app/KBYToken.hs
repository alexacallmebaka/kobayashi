{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards   #-}

module KBYToken
    ( KBYToken(..)
    , KBYWithInfo(..)
    , KBYStream(..)
    ) where

import qualified Data.Text as T
import qualified Data.List.NonEmpty as NE
import Data.Proxy (Proxy(..))
import Data.List

import Text.Megaparsec
import Text.Megaparsec.Pos
import Text.Megaparsec.Stream

-- tokens {{{1
data KBYToken = BeginHeader
              | BeginSubheader
              | Bold 
              | Italic 
              | EndOfBlock 
              | TextChar deriving (Eq, Show, Ord)

data KBYWithInfo = KBYWithInfo
    { startPos :: SourcePos
    , asTxt :: T.Text
    , innerToken :: KBYToken
    } deriving (Eq, Show, Ord)

data KBYStream = KBYStream { unStream :: [KBYWithInfo] } deriving (Eq, Show)

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

instance VisualStream KBYStream where
    showTokens Proxy = foldl (\acc x -> acc ++ (T.unpack . asTxt $ x)) ""
    tokensLength Proxy = foldl (\acc x -> acc + (T.length . asTxt $ x)) 0 

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
                                            (pre, post) = splitAt offset $ unStream pstateInput
                                            newSourcePos = startPos . head $ pre
                                            line = let getLine = sourceLine . startPos
                                                       startLine = getLine . head . unStream $ pstateInput
                                                       restOfLine = fst $ takeWhile_ (\x -> (getLine x) == startLine ) pstateInput
                                                   in foldl (\acc x -> acc ++ (T.unpack . asTxt $ x)) "" restOfLine
