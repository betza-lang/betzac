-- | Legs which have been asked for more than any one move can satisfy.
module Betzac.Semantic.Moveset (analysisEmptyMoveset) where

import Betzac.AST
import Betzac.AST.Origin (HasOrigin (..), Origin (Contradicted))
import Betzac.Diagnostic

analysisEmptyMoveset :: BetzaProgram Ds -> Pass ()
analysisEmptyMoveset prog = mapM_ warnEmpty (universeOf prog)

{- | One diagnostic per leg, not per modifier: a contradiction is a property of the
pair, and reporting each half would say the same thing twice.
-}
warnEmpty :: Qualification Ds -> Pass ()
warnEmpty (Qualification _ bs) = case filter contradictory bs of
    [] -> return ()
    (b : _) -> emitWarningAt (EmptyMoveset (cause b)) b
  where
    contradictory x = origin x == Contradicted
    cause b =
        "the leg is asked to be "
            <> describe b
            <> " and, by what encloses it, something no leg can be at the same time"

describe :: Behaviour Ds -> String
describe (Behaviour k _ _) = case k of
    Capture _ -> "a capture"
    Move _ -> "quiet"
    Leap _ -> "a leap"
    Initial _ -> "an opening move"
    Jump _ -> "a visit"
    NoJump _ -> "a slide"
    Hop _ -> "a hop"
