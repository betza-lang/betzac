module Betzac.AST.Utils (exprOf) where

import Betzac.AST.Types (BetzaExpr, BetzaStmt (Anonymous, Assign))

exprOf :: BetzaStmt p -> BetzaExpr p
exprOf (Assign _ e _) = e
exprOf (Anonymous e _) = e
