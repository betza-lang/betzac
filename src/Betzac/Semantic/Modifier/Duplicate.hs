module Betzac.Semantic.Modifier.Duplicate (analysisDuplicateModifiers) where

import Betzac.AST
import Betzac.Diagnostic

import Data.List (inits)

analysisDuplicateModifiers :: [Modifier Ps] -> Pass ()
analysisDuplicateModifiers ms =
    mapM_ warnDuplicate [m | (m, earlier) <- zip ms (inits ms), any (stripEq m) earlier]
  where
    warnDuplicate :: Modifier Ps -> Pass ()
    warnDuplicate = emitWarningAt $ RedundantModifier "duplicate"
