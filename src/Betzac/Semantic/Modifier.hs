module Betzac.Semantic.Modifier (analysisModifiers) where

import Betzac.AST
import Betzac.Diagnostic (Pass)
import Betzac.Semantic.Modifier.Duplicate (analysisDuplicateModifiers)
import Betzac.Semantic.Modifier.Subsume (analysisSubsumedModifiers)

analysisModifiers :: ModifierExpr Ps -> Pass ()
analysisModifiers m = let ms = modifiers m in analysisDuplicateModifiers ms >> analysisSubsumedModifiers ms
