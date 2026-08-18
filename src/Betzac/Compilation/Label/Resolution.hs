module Betzac.Compilation.Label.Resolution (resolveLabelBody) where

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaExpr, Label (Leaper))
import Betzac.AST.Utils (exprLabels)
import Betzac.Compilation.Context (ResolvedDef (..), edExpr)
import Betzac.Compilation.Label.Scope (LabelTable, labelText)
import Betzac.Diagnostic
import Betzac.Span (HasSpan (getSpan))

import qualified Data.Map as Map

type Trail = [String] -- labels currently being expanded on a path

resolveLabelBody :: LabelTable ResolvedDef -> FilePath -> String -> BetzaExpr Ps -> [SemanticProblem]
resolveLabelBody eff file lbl = walk [lbl]
  where
    walk :: Trail -> BetzaExpr Ps -> [SemanticProblem]
    walk trail = concatMap (resolve trail) . exprLabels

    -- A leaper label (e.g. `:1,1:`) is a literal geometric value, not a name.
    resolve :: Trail -> Label Ps -> [SemanticProblem]
    resolve _ (Leaper _ _ _) = []
    resolve trail ref = case Map.lookup name eff of
        Nothing -> [mkProblem Error UnresolvedLabel $ getSpan ref]
        Just (ResolvedDef from _) | from /= file -> [] -- imported: not our problem
        Just (ResolvedDef _ def)
            | name `elem` trail -> [mkProblem Error (CircularLabel $ dropWhile (/= name) trail ++ [name]) $ getSpan ref]
            | otherwise -> foldMap (walk $ trail ++ [name]) (edExpr def)
      where
        name = labelText ref
