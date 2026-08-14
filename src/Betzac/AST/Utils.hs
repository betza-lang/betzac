module Betzac.AST.Utils (exprOf) where

import Betzac.AST.Types (BetzaExpr, BetzaStmt (Assign, LabelRef))

-- | The expression a statement defines, if any — a bare label reference (@label;@)
-- doesn't introduce one of its own.
exprOf :: BetzaStmt p -> Maybe (BetzaExpr p)
exprOf (Assign _ e _) = Just e
exprOf (LabelRef _ _) = Nothing
