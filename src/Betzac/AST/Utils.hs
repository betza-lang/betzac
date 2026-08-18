module Betzac.AST.Utils (
    directiveOf,
    stmtOf,
    exprOf,
    isOverride,
    isExported,
    stmtLabel,
    definedLabel,
    referencedLabel,
    exprLabels,
) where

import Betzac.AST.Generic (universeOf)
import Betzac.AST.Phases (Ps)
import Betzac.AST.Types (
    BetzaExpr,
    BetzaStmt (..),
    Directive (..),
    Label,
    QualifiedStmt (..),
 )

directiveOf :: QualifiedStmt p -> Directive p
directiveOf (Override d _) = d
directiveOf (Plain d _) = d

-- | The statement a qualified statement carries, unless it's a @using@.
stmtOf :: QualifiedStmt p -> Maybe (BetzaStmt p)
stmtOf qs = case directiveOf qs of
    Export stmt _ -> Just stmt
    Bare stmt _ -> Just stmt
    Using _ _ -> Nothing

-- | The expression a statement binds; a bare label reference binds none.
exprOf :: BetzaStmt p -> Maybe (BetzaExpr p)
exprOf (Assign _ e _) = Just e
exprOf (LabelRef _ _) = Nothing

isOverride :: QualifiedStmt p -> Bool
isOverride (Override _ _) = True
isOverride (Plain _ _) = False

isExported :: QualifiedStmt p -> Bool
isExported qs = case directiveOf qs of
    Export _ _ -> True
    _ -> False

-- | The label a statement names, whether it defines it or only references it.
stmtLabel :: BetzaStmt p -> Label p
stmtLabel (Assign lbl _ _) = lbl
stmtLabel (LabelRef lbl _) = lbl

definedLabel :: BetzaStmt p -> Maybe (Label p)
definedLabel (Assign lbl _ _) = Just lbl
definedLabel (LabelRef _ _) = Nothing

referencedLabel :: BetzaStmt p -> Maybe (Label p)
referencedLabel (LabelRef lbl _) = Just lbl
referencedLabel (Assign _ _ _) = Nothing

-- | Every label occurring in an expression; they are all references.
exprLabels :: BetzaExpr Ps -> [Label Ps]
exprLabels = universeOf
