module Betzac.Compilation.Label.Resolution (resolveLabelBody, checkUnresolvedRefs) where

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaExpr, BetzaProgram, Label (Leaper), Labelling)
import Betzac.AST.Utils (exprLabels)
import Betzac.Compilation.Context (ResolvedDef (..), edExpr)
import Betzac.Compilation.Label.Scope (LabelTable, labelText, unexportedLabelRefs)
import Betzac.Diagnostic
import Betzac.Span (HasSpan (getSpan))

import qualified Data.Map as Map
import qualified Data.Set as Set

type Trail = [Labelling] -- labels currently being expanded on a path
type Seen = Set.Set Labelling -- definitions already expanded, on any path

-- | A bare label reference naming something that is nowhere in scope.
checkUnresolvedRefs :: LabelTable ResolvedDef -> BetzaProgram Ps -> [SemanticProblem]
checkUnresolvedRefs eff prog =
    [ mkProblem Error UnresolvedLabel $ getSpan stmt
    | (stmt, name) <- unexportedLabelRefs prog
    , not $ Map.member name eff
    ]

{- | Every unresolved or circular reference a label's body leads to, directly or
through the definitions it names. Each definition is expanded at most once: a label
several of them share would otherwise be reported once per path that reaches it, which
grows exponentially with the depth of the sharing.
-}
resolveLabelBody :: LabelTable ResolvedDef -> FilePath -> Labelling -> BetzaExpr Ps -> [SemanticProblem]
resolveLabelBody eff file lbl = snd . walk (Set.singleton lbl) [lbl]
  where
    walk :: Seen -> Trail -> BetzaExpr Ps -> (Seen, [SemanticProblem])
    walk seen trail = foldl' step (seen, []) . exprLabels
      where
        step (seen', probs) ref = (probs ++) <$> resolve seen' trail ref

    resolve :: Seen -> Trail -> Label Ps -> (Seen, [SemanticProblem])
    -- A leaper label (e.g. `:1,1:`) is a literal geometric value, not a name.
    resolve seen _ (Leaper _ _ _) = (seen, [])
    resolve seen trail ref = case Map.lookup name eff of
        Nothing -> (seen, [mkProblem Error UnresolvedLabel $ getSpan ref])
        Just (ResolvedDef from _) | from /= file -> (seen, []) -- imported: not our problem
        Just (ResolvedDef _ def)
            | name `elem` trail -> (seen, [mkProblem Error (CircularLabel $ dropWhile (/= name) trail ++ [name]) $ getSpan ref])
            | name `Set.member` seen -> (seen, []) -- another path got there first
            | otherwise -> maybe (seen, []) (walk (Set.insert name seen) (trail ++ [name])) (edExpr def)
      where
        name = labelText ref
