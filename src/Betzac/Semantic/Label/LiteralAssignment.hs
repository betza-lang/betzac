module Betzac.Semantic.Label.LiteralAssignment (analysisLiteralAssignment) where

import Betzac.AST
import Betzac.Diagnostic

analysisLiteralAssignment :: BetzaStmt Ps -> Pass ()
analysisLiteralAssignment s@(Assign (Leaper _ _ _) _ _) =
    emitErrorAt (InvalidStatement "cannot assign to a leaper literal") s
analysisLiteralAssignment _ = pure ()
