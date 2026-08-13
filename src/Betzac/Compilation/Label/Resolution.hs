module Betzac.Compilation.Label.Resolution (resolveLabelBody) where

import Betzac.AST
import Betzac.Compilation.Context (ExportedDef (edStmt), ResolvedDef (..))
import Betzac.Compilation.Label.Scope
import Betzac.Semantic.Core
import Betzac.Span (HasSpan (getSpan))
import Data.Foldable (Foldable (toList))
import qualified Data.Map as Map

type Trail = [String] -- labels currently being expanded on a path

resolveLabelBody :: LabelTable ResolvedDef -> FilePath -> String -> BetzaExpr Ps -> [SemanticProblem]
resolveLabelBody eff file lbl = resolveLabelBody' eff file [lbl]

resolveLabelBody' :: LabelTable ResolvedDef -> FilePath -> Trail -> BetzaExpr Ps -> [SemanticProblem]
resolveLabelBody' eff file trail (BetzaExpr ce _) = resolveChainExpr ce
  where
    resolveChainExpr (ChainExpr ue mcl _) = resolveUnionExpr ue ++ concat (resolveChainLeg <$> toList mcl)
    resolveChainLeg (ChainLeg _ ce' _) = resolveChainExpr ce'
    resolveUnionExpr (UnionExpr mes _) = concat $ resolveModifierExpr <$> toList mes
    resolveModifierExpr (ModifierExpr _ _ ee _) = resolveExponentExpr ee
    resolveExponentExpr (ExponentExpr ae _ _) = resolveAtomExpr eff file trail ae

resolveAtomExpr :: LabelTable ResolvedDef -> FilePath -> Trail -> AtomExpr Ps -> [SemanticProblem]
resolveAtomExpr eff file trail (Paren expr _) = resolveLabelBody' eff file trail expr
resolveAtomExpr eff file trail (From lbl _) =
    let name = labelText lbl
     in case Map.lookup name eff of
            Nothing -> [mkProblem Error UnresolvedLabel $ getSpan lbl]
            Just (ResolvedDef from _) | from /= file -> [] -- imported: not our problem
            Just (ResolvedDef _ def)
                | name `elem` trail -> [mkProblem Error (CircularLabel $ dropWhile (/= name) trail ++ [name]) $ getSpan lbl]
                | otherwise -> resolveLabelBody' eff file (trail ++ [name]) $ exprOf $ edStmt def
  where
    exprOf (Assign _ e _) = e
    exprOf (Anonymous e _) = e
