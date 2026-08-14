module Betzac.Compilation.Label.Liveness (checkDeadLabels) where

import Betzac.AST.Phases as B
import Betzac.AST.Types as B
import Betzac.AST.Utils
import Betzac.Compilation.Context
import Betzac.Compilation.Label.Scope
import Betzac.Diagnostic
import Betzac.Span

import Data.Foldable
import qualified Data.Map as Map
import qualified Data.Set as Set

checkDeadLabels :: LabelTable ResolvedDef -> LabelTable ExportedDef -> FilePath -> [SemanticProblem]
checkDeadLabels eff exported file =
    [ mkProblem Warning (UnusedLabel name) (getSpan $ edStmt def)
    | (name, ResolvedDef from def) <- Map.toList eff
    , from == file
    , not $ name `Set.member` live
    ]
  where
    roots = Map.keysSet exported -- exported names are live by definition
    live = foldl' (\l def -> maybe l (walkExpr eff file l) (exprOf $ edStmt def)) roots $ Map.elems exported

walkExpr :: LabelTable ResolvedDef -> FilePath -> Set.Set Labelling -> BetzaExpr Ps -> Set.Set Labelling
walkExpr eff file live (BetzaExpr ce _) = walkChainExpr live ce
  where
    walkChainExpr l (ChainExpr ue mcl _) = foldl' walkChainLeg (walkUnionExpr l ue) (toList mcl)
    walkChainLeg l (ChainLeg _ ce' _) = walkChainExpr l ce'
    walkUnionExpr l (UnionExpr mes _) = foldl' walkModifierExpr l (toList mes)
    walkModifierExpr l (ModifierExpr _ _ ee _) = walkExponentExpr l ee
    walkExponentExpr l (ExponentExpr ae _ _) = walkAtomExpr eff file l ae

walkAtomExpr :: LabelTable ResolvedDef -> FilePath -> Set.Set Labelling -> AtomExpr Ps -> Set.Set Labelling
walkAtomExpr eff file live (Paren expr _) = walkExpr eff file live expr
walkAtomExpr eff file live (From lbl _) =
    let name = labelText lbl
     in case Map.lookup name eff of
            Nothing -> live -- it's not even defined, should be caught by the resolution pass as unresolved instead
            Just (ResolvedDef from _) | from /= file -> live -- imported, so cannot be considered dead in this file
            Just (ResolvedDef _ def)
                | name `Set.member` live -> live
                | otherwise -> maybe live (walkExpr eff file (Set.insert name live)) (exprOf $ edStmt def)
