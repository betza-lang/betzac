{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Betzac.Parser.BetzaTokenStream (BetzaTokenStream (..)) where

import qualified Betzac.Token as B

import Betzac.Located
import qualified Data.List as DL
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Text.Megaparsec

data BetzaTokenStream = BetzaTokenStream
    { source :: String
    , unBetzaTokenStream :: [Located B.Token]
    }
    deriving (Eq, Show)

instance Stream BetzaTokenStream where
    type Token BetzaTokenStream = Located B.Token
    type Tokens BetzaTokenStream = [Located B.Token]
    tokenToChunk Proxy x = [x]
    tokensToChunk Proxy xs = xs
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    take1_ (BetzaTokenStream _ []) = Nothing
    take1_ (BetzaTokenStream str (t : ts)) =
        Just
            ( t
            , BetzaTokenStream (drop (tokensLength pxy (t :| [])) str) ts
            )
    takeN_ n (BetzaTokenStream str s)
        | n <= 0 = Just ([], BetzaTokenStream str s)
        | null s = Nothing
        | otherwise =
            let (x, s') = splitAt n s
             in case NE.nonEmpty x of
                    Nothing -> Just (x, BetzaTokenStream str s')
                    Just nex -> Just (x, BetzaTokenStream (drop (tokensLength pxy nex) str) s')
    takeWhile_ f (BetzaTokenStream str s) =
        let (x, s') = DL.span f s
         in case NE.nonEmpty x of
                Nothing -> (x, BetzaTokenStream str s')
                Just nex -> (x, BetzaTokenStream (drop (tokensLength pxy nex) str) s')

instance VisualStream BetzaTokenStream where
    showTokens Proxy =
        DL.intercalate " "
            . NE.toList
            . fmap (B.showToken . tokenVal)

instance TraversableStream BetzaTokenStream where
    reachOffset o PosState{..} =
        ( Just (prefix ++ restOfLine)
        , PosState
            { pstateInput =
                BetzaTokenStream
                    { source = postStr
                    , unBetzaTokenStream = post
                    }
            , pstateOffset = max pstateOffset o
            , pstateSourcePos = newSourcePos
            , pstateTabWidth = pstateTabWidth
            , pstateLinePrefix = prefix
            }
        )
      where
        prefix =
            if sameLine
                then pstateLinePrefix ++ preLine
                else preLine
        sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
        newSourcePos = case post of
            [] -> case unBetzaTokenStream pstateInput of
                [] -> pstateSourcePos
                xs -> endPos $ last xs
            (x : _) -> startPos x
        (pre, post) = splitAt (o - pstateOffset) (unBetzaTokenStream pstateInput)
        (preStr, postStr) = splitAt tokensConsumed $ source pstateInput
        preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
        tokensConsumed =
            case NE.nonEmpty pre of
                Nothing -> 0
                Just nePre -> tokensLength pxy nePre
        restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy BetzaTokenStream
pxy = Proxy
