module Betzac.Compilation.Label.Resolution (resolveLabelBody, checkUnresolvedRefs) where

import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (BetzaExpr, BetzaProgram, Label (Leaper), Labelling)
import Betzac.AST.Utils (exprLabels)
import Betzac.Compilation.Context (ResolvedDef (..), sdExpr)
import Betzac.Compilation.Label.Scope (LabelTable, labelText, unexportedLabelRefs)
import Betzac.Diagnostic
import Betzac.Span (HasSpan (getSpan))

import qualified Data.Map as Map
import qualified Data.Set as Set

type Trail = [Labelling] -- the path walked from the label being checked
type Seen = Set.Set Labelling -- definitions already expanded, on any path

-- | A bare label reference naming something that is nowhere in scope.
checkUnresolvedRefs :: LabelTable ResolvedDef -> BetzaProgram Ps -> [SemanticProblem]
checkUnresolvedRefs eff prog =
    [ mkProblem Error UnresolvedLabel $ getSpan stmt
    | (stmt, name) <- unexportedLabelRefs prog
    , not $ Map.member name eff
    ]

-- | Every unresolved or circular reference a label's body leads to, directly or through the definitions it names.
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
            -- Only a cycle back through lbl is ours; one further down belongs to the
            -- definition it runs through, not to every consumer of it.
            | name == lbl -> (seen, [mkProblem Error (CircularLabel $ trail ++ [name]) $ getSpan ref])
            | name `Set.member` seen -> (seen, []) -- expanded already, or a cycle of its own
            | otherwise -> maybe (seen, []) (walk (Set.insert name seen) (trail ++ [name])) (sdExpr def)
      where
        name = labelText ref
