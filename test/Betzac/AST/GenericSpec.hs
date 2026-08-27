{-# LANGUAGE ScopedTypeVariables #-}

{- | The generic walk against the SYB one it replaced. 'Data' instances are still
derived for the AST, so the old implementation can be kept here verbatim as the
reference and held to it on generated programs.
-}
module AST.GenericSpec (spec) where

import Data.Data (Data, Typeable, cast, gmapQr)
import qualified Data.Text as T

import Betzac.AST.Generic (Collector (..), universeBy, universeOf)
import Betzac.AST.Phases (Ps, Stripped)
import Betzac.AST.Types
import Betzac.AST.Utils (stmtOf)
import Betzac.Pipeline (PipelineResult (..), fromScratch)
import Betzac.Span (Span)
import Betzac.Utils.Unparse (unparse)
import Data.Maybe (mapMaybe)

import Lexer.LexerQC (unlex)
import Parser.ParserHedgehog (genProgram)

import Hedgehog
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

{- | The SYB traversal 'Betzac.AST.Generic.universeOf' replaced, kept as the
reference for what the new one has to reproduce exactly.
-}
universeOfData :: forall a b. (Data a, Typeable b) => a -> [b]
universeOfData x = go x []
  where
    go :: forall c. (Data c) => c -> [b] -> [b]
    go y acc = case cast y of
        Just v -> v : rest
        Nothing -> rest
      where
        rest
            | isSpan y = acc
            | otherwise = gmapQr ($) acc go y

    isSpan :: forall c. (Data c) => c -> Bool
    isSpan y = case (cast y :: Maybe Span) of
        Just _ -> True
        Nothing -> False

-- | A generated program rendered to source and parsed back, so it carries real spans.
parsedBack :: BetzaProgram Stripped -> BetzaProgram Ps
parsedBack prog = case fromScratch "<test>" (T.pack $ unlex $ unparse prog) of
    Right pr -> maybe [] fst (parseResult pr)
    Left _ -> []

spec :: Spec
spec = describe "AST.Generic" $ do
    describe "universeOf" $ do
        it "collects what the SYB traversal collected, on a parsed program" $
            hedgehog $ do
                prog <- forAll genProgram
                let ps = parsedBack prog
                universeOf ps === (universeOfData ps :: [Direction Ps])
                universeOf ps === (universeOfData ps :: [Behaviour Ps])
                universeOf ps === (universeOfData ps :: [ChainOperator Ps])
                universeOf ps === (universeOfData ps :: [Label Ps])
                universeOf ps === (universeOfData ps :: [ModifierExpr Ps])
                universeOf ps === (universeOfData ps :: [Exponent Ps])
                universeOf ps === (universeOfData ps :: [QualifiedStmt Ps])

        it "collects what the SYB traversal collected, on a stripped program" $
            hedgehog $ do
                prog <- forAll genProgram
                universeOf prog === (universeOfData prog :: [Direction Stripped])
                universeOf prog === (universeOfData prog :: [ModifierExpr Stripped])
                universeOf prog === (universeOfData prog :: [Label Stripped])

        it "finds every statement in the directives alone, no descent needed" $
            hedgehog $ do
                prog <- forAll genProgram
                let ps = parsedBack prog
                mapMaybe stmtOf ps === (universeOf ps :: [BetzaStmt Ps])

    describe "universeBy" $
        it "gathers several types in one descent, in the order separate walks would meet them" $
            hedgehog $ do
                prog <- forAll genProgram
                let ps = parsedBack prog
                    -- Tagged so a merged walk's interleaving stays visible.
                    tagged = Collector $ \node acc -> case cast node :: Maybe (Direction Ps) of
                        Just d -> Left d : acc
                        Nothing -> case cast node :: Maybe (Label Ps) of
                            Just l -> Right l : acc
                            Nothing -> acc
                    merged = universeBy tagged ps :: [Either (Direction Ps) (Label Ps)]
                [d | Left d <- merged] === (universeOf ps :: [Direction Ps])
                [l | Right l <- merged] === (universeOf ps :: [Label Ps])

    describe "universeOf" $
        it "yields a span where one is asked for, without descending into it" $
            hedgehog $ do
                prog <- forAll genProgram
                let ps = parsedBack prog
                universeOf ps === (universeOfData ps :: [Span])
