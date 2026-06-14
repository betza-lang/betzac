{-# OPTIONS_GHC -Wno-orphans #-}

module Arbitrary () where

import Betzac.Alphabet.Expr
import Betzac.Token (Token (..))
import Data.List (intercalate)
import Test.QuickCheck

descriptor :: Gen String
descriptor = intercalate [space] <$> resize 4 (listOf1 $ resize 7 $ listOf1 $ elements (',' : alphanum))

exprToken :: Gen Token
exprToken = sized $ \n ->
    let cap k = fromInteger $ min k (fromIntegral (maxBound :: Int))
     in oneof
            [ TokAtom <$> elements upper
            , TokDescriptor <$> descriptor
            , TokDirection <$> elements direction
            , TokBehaviour <$> elements behaviour
            , oneof [pure TokLParen, pure TokRParen]
            , oneof [pure TokLBracket, pure TokRBracket]
            , oneof [pure TokLBrace, pure TokRBrace]
            , oneof [pure TokLAngle, pure TokRAngle]
            , oneof [pure TokChainStep, pure TokChainSequence]
            , pure TokBang
            , pure TokSlippery
            , frequency
                [ (1, pure $ TokNumber 0)
                , (3, TokNumber <$> choose (0, let n' = (n + 1) `div` 5 in cap (10 ^ n')))
                ]
            , pure TokComma
            ]

instance Arbitrary Token where
    arbitrary = exprToken
    shrink (TokNumber n) = TokNumber <$> shrink n
    shrink (TokDescriptor d) = TokDescriptor <$> filter validDescriptor (shrink d)
      where
        validDescriptor s = not (null s)
    shrink _ = []