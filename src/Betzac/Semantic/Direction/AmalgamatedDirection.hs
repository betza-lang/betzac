module Betzac.Semantic.Direction.AmalgamatedDirection (analysisAmalgamatedDirection) where

import Betzac.AST.Phases
import Betzac.AST.Types
import Betzac.Diagnostic

import Betzac.AST.Strip
import Betzac.Debug.PrettyPrint (summarize)
import Control.Monad (when)

invalid :: Direction Ps -> Bool
invalid (All _) = True
invalid (Sideway _) = True
invalid (Vertically _) = True
invalid _ = False

invalidateBadAmalgamatedDirection :: DirectionModifier Ps -> Pass ()
invalidateBadAmalgamatedDirection (Amalgamated d1 d2 _) = do
    when (invalid d1) $ emitErrorAt (problemKind d1) d1
    when (invalid d2) $ emitErrorAt (problemKind d2) d2
  where
    problemKind d = InvalidValue $ show d ++ " in amalgamated direction"
invalidateBadAmalgamatedDirection (Single _ _) = return ()

badCombination :: Direction Ps -> Direction Ps -> Bool
badCombination (Forward _) = (== Backward ()) . strip
badCombination (Backward _) = (== Forward ()) . strip
badCombination (Leftward _) = (== Rightward ()) . strip
badCombination (Rightward _) = (== Leftward ()) . strip
badCombination _ = const False

invalidateBadAmalgamatedDirectionCombination :: DirectionModifier Ps -> Pass ()
invalidateBadAmalgamatedDirectionCombination n@(Amalgamated d1 d2 _) = when (badCombination d1 d2) $ emitErrorAt (problemKind d1 d2) n
  where
    problemKind x y = InvalidValue $ summarize x ++ " and " ++ summarize y ++ " are contradicting"
invalidateBadAmalgamatedDirectionCombination (Single _ _) = return ()

analysisAmalgamatedDirection :: DirectionModifier Ps -> Pass ()
analysisAmalgamatedDirection dm = invalidateBadAmalgamatedDirection dm >> invalidateBadAmalgamatedDirectionCombination dm
