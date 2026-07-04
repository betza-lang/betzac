module Betzac.Semantic.Direction.All () where

-- import Betzac.AST
-- import Betzac.Located
-- import Betzac.Semantic.AnalysisPass
-- import Control.Monad (when)
-- import qualified Data.List.NonEmpty as NE

-- invalidateAllInFirstLeg :: Located BetzaExpr -> Pass ()
-- invalidateAllInFirstLeg node =
--     let BetzaExpr (ChainExpr (UnionExpr ms) _) = tokenVal node
--      in mapM_ (invalidateAllInModifierExpr node) $ NE.toList ms

-- invalidateAllInModifierExpr :: Located BetzaExpr -> ModifierExpr -> Pass ()
-- invalidateAllInModifierExpr node modExpr =
--     let ms = modifiers modExpr
--      in when (hasAll ms) $ emitErrorAt node $ AllInFirstLeg
--   where
--     hasAll = any (== Directional (Single All))

-- analysisAll :: Located BetzaExpr -> Pass ()
-- analysisAll = invalidateAllInFirstLeg
