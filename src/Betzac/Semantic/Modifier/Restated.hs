-- | What a leg writes that the language would have supplied for it anyway.
module Betzac.Semantic.Modifier.Restated (analysisRestatedModifiers) where

import Betzac.AST
import Betzac.AST.Origin (HasOrigin (..), Origin (Restated))
import Betzac.Diagnostic

analysisRestatedModifiers :: BetzaProgram Ds -> Pass ()
analysisRestatedModifiers prog = mapM_ warnRestated (universeOf prog)

{- | One diagnostic per half of the modifier string, not per node: the pair of a
final leg's @cm@ is a single restatement, and reporting each letter would say it twice.
-}
warnRestated :: Qualification Ds -> Pass ()
warnRestated (Qualification ds bs) =
    firstRestated ds `emitEach` "restates the direction the leg is read as carrying"
        >> firstRestated bs `emitEach` "restates what a final leg carries anyway"
  where
    emitEach node cause = mapM_ (emitWarningAt (RedundantModifier cause)) node

firstRestated :: (Foldable t, HasOrigin a) => t a -> Maybe a
firstRestated = foldr keep Nothing
  where
    keep x acc = if origin x == Restated then Just x else acc
