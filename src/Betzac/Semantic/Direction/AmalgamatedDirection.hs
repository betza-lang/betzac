module Betzac.Semantic.Direction.AmalgamatedDirection () where

-- import Betzac.AST
-- import Betzac.Located
-- import Betzac.Semantic.AnalysisPass

-- import Control.Monad (when)

-- invalid :: Direction -> Bool
-- invalid All = True
-- invalid Sideway = True
-- invalid Vertically = True
-- invalid _ = False

-- invalidateBadAmalgamatedDirection :: Located DirectionModifier -> Pass ()
-- invalidateBadAmalgamatedDirection node = case tokenVal node of
--     Amalgamated d1 d2 -> do
--         when (invalid d1) $ emitErrorAt node $ InvalidValue $ show d1 ++ " in amalgamated direction"
--         when (invalid d2) $ emitErrorAt node $ InvalidValue $ show d2 ++ " in amalgamated direction"
--     Single _ -> return ()

-- badCombination :: Direction -> Direction -> Bool
-- badCombination Forward = (== Backward)
-- badCombination Backward = (== Forward)
-- badCombination Leftward = (== Rightward)
-- badCombination Rightward = (== Leftward)
-- badCombination _ = const False

-- invalidateBadAmalgamatedDirectionCombination :: Located DirectionModifier -> Pass ()
-- invalidateBadAmalgamatedDirectionCombination node = case tokenVal node of
--     Amalgamated d1 d2 -> when (badCombination d1 d2) $ emitErrorAt node $ InvalidValue "contradicting directions"
--     Single _ -> return ()

-- analysisAmalgamatedDirection :: Located DirectionModifier -> Pass ()
-- analysisAmalgamatedDirection = invalidateBadAmalgamatedDirection >> invalidateBadAmalgamatedDirectionCombination
