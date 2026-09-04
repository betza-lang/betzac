{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

-- | States what betza source leaves implicit, so that no later phase re-derives it.
module Betzac.AST.Desugar (desugar) where

import Betzac.AST
import Betzac.AST.Origin (Origin (..))
import Betzac.Span (HasSpan (..), Span (Generated))
import Data.List (partition)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE

-- | Which direction a leg carrying none is read as carrying.
data Inherited
    = Ahead
    | Omnidirectional
    deriving (Eq)

{- | Whether a leg ends the move. Ordered by how much a branch can still add, so
that a leg is no more final than the chain enclosing it.
-}
data Finality
    = NeverFinal
    | SometimesFinal
    | AlwaysFinal
    deriving (Eq, Ord)

data Leg = Leg
    { legImplied :: Inherited
    , legFinality :: Finality
    , legCarried :: [Behaviour Ds]
    -- ^ Behaviours from an enclosing level, to be applied where the move can end.
    }

desugar :: BetzaProgram Ps -> BetzaProgram Ds
desugar = map qualifiedStmt

qualifiedStmt :: QualifiedStmt Ps -> QualifiedStmt Ds
qualifiedStmt (Override d x) = Override (directive d) (written x)
qualifiedStmt (Plain d x) = Plain (directive d) (written x)

directive :: Directive Ps -> Directive Ds
directive (Using f x) = Using f (written x)
directive (Export s x) = Export (stmt s) (written x)
directive (Bare s x) = Bare (stmt s) (written x)

stmt :: BetzaStmt Ps -> BetzaStmt Ds
stmt (Assign l e x) = Assign (label l) (expr wholeMove e) (written x)
stmt (LabelRef l x) = LabelRef (label l) (written x)

-- | A definition stands on its own, so its last leg ends the move.
wholeMove :: Leg
wholeMove = Leg Omnidirectional AlwaysFinal []

expr :: Leg -> BetzaExpr Ps -> BetzaExpr Ds
expr leg (BetzaExpr c x) = BetzaExpr (chain leg c) (written x)

chain :: Leg -> ChainExpr Ps -> ChainExpr Ds
chain leg (ChainExpr u Nothing x) =
    ChainExpr (union leg u) Nothing (written x)
chain leg (ChainExpr u (Just cl@(ChainLeg op _ _)) x) =
    ChainExpr (union headLeg u) (Just (chainLeg leg cl)) (written x)
  where
    headLeg = leg{legFinality = min (legFinality leg) (finalityBefore op)}

-- | A leg the chain may continue past has not ended the move in every branch.
finalityBefore :: ChainOperator Ps -> Finality
finalityBefore (ChainOperator _ (Mandatory _) _) = NeverFinal
finalityBefore (ChainOperator _ (Choose _) _) = SometimesFinal
finalityBefore (ChainOperator _ (IffUnblocked _) _) = SometimesFinal

chainLeg :: Leg -> ChainLeg Ps -> ChainLeg Ds
chainLeg leg (ChainLeg op c x) =
    ChainLeg (chainOperator op) (chain leg{legImplied = inheritedFrom op} c) (written x)

-- | A step chain orients its continuation by the leg before it; a sequence chain does not.
inheritedFrom :: ChainOperator Ps -> Inherited
inheritedFrom (ChainOperator (Step _) _ _) = Ahead
inheritedFrom (ChainOperator (Sequence _) _ _) = Omnidirectional

union :: Leg -> UnionExpr Ps -> UnionExpr Ds
union leg (UnionExpr ms x) = UnionExpr (NE.map (modExpr leg) ms) (written x)

modExpr :: Leg -> ModifierExpr Ps -> ModifierExpr Ds
modExpr leg (ModifierExpr s ms e@(ExponentExpr atom mexp _) x) =
    ModifierExpr s (qualification here mine) (exponentExpr inside e) (written x)
  where
    here =
        Leg
            (impliedAt atom (legImplied leg))
            (withExponent mexp (finalityAt atom (legFinality leg)))
            (carriedAt atom (legCarried leg))
    -- A parenthesised chain takes the inheritance and the behaviours its wrapper did not spend.
    inside =
        leg
            { legImplied =
                if any isDirectional ms then Omnidirectional else legImplied leg
            , legCarried = legCarried leg <> [behaviour b | Behavioural b _ <- ms]
            }
    -- A behaviour written on a wrapper qualifies the leg that ends the chain, not the wrapper.
    mine = case atom of
        Paren _ _ -> filter isDirectional ms
        From _ _ -> ms

-- | A wrapper around a chain constrains its head only, so it claims no direction of its own.
impliedAt :: AtomExpr Ps -> Inherited -> Inherited
impliedAt (Paren _ _) _ = Omnidirectional
impliedAt (From _ _) i = i

-- | The head a wrapper constrains is not the leg that ends the move; the leg inside is.
finalityAt :: AtomExpr Ps -> Finality -> Finality
finalityAt (Paren _ _) _ = NeverFinal
finalityAt (From _ _) f = f

-- | A wrapper passes what it carries inward rather than spending it on itself.
carriedAt :: AtomExpr Ps -> [Behaviour Ds] -> [Behaviour Ds]
carriedAt (Paren _ _) _ = []
carriedAt (From _ _) cs = cs

{- | Repetition leaves the number of legs to a branch, so which copy ends the move
is a branch's answer, not the head's.
-}
withExponent :: Maybe (Exponent Ps) -> Finality -> Finality
withExponent Nothing f = f
withExponent (Just _) f = min f SometimesFinal

isDirectional :: Modifier Ps -> Bool
isDirectional = \case Directional _ _ -> True; _ -> False

exponentExpr :: Leg -> ExponentExpr Ps -> ExponentExpr Ds
exponentExpr inside (ExponentExpr a me x) =
    ExponentExpr (atomExpr inside a) (fmap exponent' me) (written x)

atomExpr :: Leg -> AtomExpr Ps -> AtomExpr Ds
atomExpr inside (Paren e x) = Paren (expr inside e) (written x)
atomExpr _ (From l x) = From (label l) (written x)

{- | An exponent's joints are legs too: they orient themselves ahead of their
predecessor, and are declinable unless an operator says otherwise.
-}
exponent' :: Exponent Ps -> Exponent Ds
exponent' (Exponent mop ms k x) = Exponent joint (qualification joints ms) kind (written x)
  where
    joints = Leg Ahead SometimesFinal []
    (joint, kind) = case k of
        -- Travelling as far as the run allows is the braced modality under another name.
        Slippery ext -> (maximalJoint (getSpan ext), Infinite (written ext))
        _ -> (maybe optionalJoint chainOperator mop, exponentKind k)

optionalJoint :: ChainOperator Ds
optionalJoint = ChainOperator (Step implied) (Choose implied) implied

maximalJoint :: Span -> ChainOperator Ds
maximalJoint s = ChainOperator (Step ext) (IffUnblocked ext) ext
  where
    ext = DsX s Implied

{- | The modifier string, with whatever the language would have supplied now stated,
and whatever restates it marked where it stands.
-}
qualification :: Leg -> [Modifier Ps] -> Qualification Ds
qualification leg ms = Qualification directions behaviours
  where
    writtenDirections = [directionModifier d | Directional d _ <- ms]
    writtenBehaviours = [behaviour b | Behavioural b _ <- ms]

    directions = case writtenDirections of
        [] -> impliedDirection (legImplied leg) :| []
        [d] | restates (legImplied leg) d -> restated d :| []
        d : ds -> d :| ds

    behaviours
        | legFinality leg /= AlwaysFinal = stated
        -- A leg permitting nothing has nothing to restate.
        | any contradictory stated = stated
        | capturing && quiet = map restateFinal stated
        -- An enclosing level has said what the leg permits, so nothing is left
        -- unsaid to supply -- including where it permits nothing at all. A
        -- behaviour that speaks to something other than capture leaves the
        -- question open, and the default still answers it.
        | any isPermission (legCarried leg) = stated
        | capturing || quiet = stated
        | otherwise = stated <> [impliedBehaviour Capture, impliedBehaviour Move]

    -- What the leg permits once an enclosing level has had its say.
    stated
        | legFinality leg == NeverFinal = writtenBehaviours
        | null (legCarried leg) = writtenBehaviours
        | otherwise = narrowedBy (legCarried leg) writtenBehaviours

    contradictory (Behaviour _ _ ext) = dsOrigin ext == Contradicted

    capturing = any (isFinalHalf Capture) stated
    quiet = any (isFinalHalf Move) stated

    restateFinal b
        | isFinalHalf Capture b || isFinalHalf Move b = restated b
        | otherwise = b

impliedDirection :: Inherited -> DirectionModifier Ds
impliedDirection Ahead = Single (Forward implied) implied
impliedDirection Omnidirectional = Single (All implied) implied

restates :: Inherited -> DirectionModifier Ds -> Bool
restates i (Single d _) = case (i, d) of
    (Ahead, Forward _) -> True
    (Omnidirectional, All _) -> True
    _ -> False
restates _ (Amalgamated _ _ _) = False

{- | A behaviour arriving from an enclosing level narrows the capture and quiet
permissions the leg states for itself; anything else it carries is simply added.
-}
narrowedBy :: [Behaviour Ds] -> [Behaviour Ds] -> [Behaviour Ds]
narrowedBy carried stated = permitted <> merged carriedRest statedRest
  where
    (carriedPermissions, carriedRest) = partition isPermission carried
    (statedPermissions, statedRest) = partition isPermission stated

    -- A leg must permit something on the capture axis, so an empty intersection
    -- there empties the move; both halves are kept and marked to say why.
    permitted
        | null statedPermissions = carriedPermissions
        | null carriedPermissions = statedPermissions
        | null kept = map contradicted (statedPermissions <> carriedPermissions)
        | otherwise = kept
    kept = concatMap (`narrowAgainst` carriedPermissions) statedPermissions

{- | Everything else the two levels ask for: a kind named at both narrows, and a
kind named at only one is simply carried through.
-}
merged :: [Behaviour Ds] -> [Behaviour Ds] -> [Behaviour Ds]
merged carried stated = concatMap keep stated <> untouched
  where
    keep s = case narrowAgainst s carried of
        [] -> [s] -- named at both but with nothing in common: not a permission, so not fatal
        bs -> bs
    untouched = [c | c <- carried, not (any (sameKind c) stated)]

{- | One behaviour narrowed by whichever of the enclosing ones names its kind.
Nothing, where they admit no case in common or where the enclosing level does not
name the kind at all -- on the capture axis that level says what may happen, so a
kind it leaves out is a kind it does not permit.
-}
narrowAgainst :: Behaviour Ds -> [Behaviour Ds] -> [Behaviour Ds]
narrowAgainst s@(Behaviour k m x) carried = case filter (sameKind s) carried of
    [] -> []
    (Behaviour _ cm _ : _) -> case narrowModality k m cm of
        Just m' -> [Behaviour k m' x]
        Nothing -> []

{- | The narrower of two modalities of one kind, or nothing where they admit no
case in common. The two cases a modality distinguishes differ by kind, so which
modality is the widest does too.
-}
narrowModality :: BehaviourKind Ds -> BehaviourModality Ds -> BehaviourModality Ds -> Maybe (BehaviourModality Ds)
narrowModality k a b
    | stripEq a b = Just a
    | widest k a = Just b
    | widest k b = Just a
    | otherwise = Nothing

-- | The modality admitting every case its kind distinguishes.
widest :: BehaviourKind Ds -> BehaviourModality Ds -> Bool
widest k m = case m of
    -- `pp` is any number of hurdles, where `p` is exactly one.
    Twice _ -> stripEq k (Hop implied)
    -- `xy` is either allegiance, where `x` and `xx` name one each.
    Any _ -> True
    Once _ -> False

-- | Whether a behaviour speaks to what may happen on the square the leg ends on.
isPermission :: Behaviour Ds -> Bool
isPermission (Behaviour k _ _) = stripEq k (Capture implied) || stripEq k (Move implied)

sameKind :: Behaviour Ds -> Behaviour Ds -> Bool
sameKind (Behaviour a _ _) (Behaviour b _ _) = stripEq a b

impliedBehaviour :: (DsX -> BehaviourKind Ds) -> Behaviour Ds
impliedBehaviour k = Behaviour (k implied) (Once ()) implied

-- | One half of what a final leg would have been qualified anyway.
isFinalHalf :: (DsX -> BehaviourKind Ds) -> Behaviour Ds -> Bool
isFinalHalf k (Behaviour kind (Once _) _) = stripEq (k implied) kind
isFinalHalf _ _ = False

-- Plain rebuilds: nothing about these is left implicit.

chainOperator :: ChainOperator Ps -> ChainOperator Ds
chainOperator (ChainOperator k m x) = ChainOperator (chainKind k) (chainModality m) (written x)

chainKind :: ChainKind Ps -> ChainKind Ds
chainKind (Step x) = Step (written x)
chainKind (Sequence x) = Sequence (written x)

chainModality :: ChainModality Ps -> ChainModality Ds
chainModality (Mandatory ()) = Mandatory ()
chainModality (Choose x) = Choose (written x)
chainModality (IffUnblocked x) = IffUnblocked (written x)

directionModifier :: DirectionModifier Ps -> DirectionModifier Ds
directionModifier (Amalgamated d1 d2 x) = Amalgamated (direction d1) (direction d2) (written x)
directionModifier (Single d x) = Single (direction d) (written x)

direction :: Direction Ps -> Direction Ds
direction (Forward x) = Forward (written x)
direction (Backward x) = Backward (written x)
direction (Leftward x) = Leftward (written x)
direction (Rightward x) = Rightward (written x)
direction (Sideway x) = Sideway (written x)
direction (Vertically x) = Vertically (written x)
direction (All x) = All (written x)

behaviour :: Behaviour Ps -> Behaviour Ds
behaviour (Behaviour k m x) = Behaviour (behaviourKind k) (behaviourModality m) (written x)

behaviourKind :: BehaviourKind Ps -> BehaviourKind Ds
behaviourKind (Capture x) = Capture (written x)
behaviourKind (Leap x) = Leap (written x)
behaviourKind (Initial x) = Initial (written x)
behaviourKind (Jump x) = Jump (written x)
behaviourKind (Move x) = Move (written x)
behaviourKind (NoJump x) = NoJump (written x)
behaviourKind (Hop x) = Hop (written x)

behaviourModality :: BehaviourModality Ps -> BehaviourModality Ds
behaviourModality (Once ()) = Once ()
behaviourModality (Twice x) = Twice (written x)
behaviourModality (Any x) = Any (written x)

exponentKind :: ExponentKind Ps -> ExponentKind Ds
exponentKind (Infinite x) = Infinite (written x)
exponentKind (Slippery x) = Infinite (written x)
exponentKind (Repeat n x) = Repeat n (written x)

label :: Label Ps -> Label Ds
label (Upper c x) = Upper c (written x)
label (Descriptor s x) = Descriptor s (written x)
label (Leaper a b x) = Leaper a b (written x)

-- Extensions

written :: PsX -> DsX
written (PsX s) = DsX s Written

implied :: DsX
implied = DsX Generated Implied

-- | Kept where it stands, so the diagnostic can point at what the reader wrote.
class Restatable a where
    remark :: Origin -> a -> a

restated, contradicted :: (Restatable a) => a -> a
restated = remark Restated
contradicted = remark Contradicted

instance Restatable DsX where
    remark o x = x{dsOrigin = o}

instance Restatable (DirectionModifier Ds) where
    remark o (Amalgamated d1 d2 x) = Amalgamated d1 d2 (remark o x)
    remark o (Single d x) = Single d (remark o x)

instance Restatable (Behaviour Ds) where
    remark o (Behaviour k m x) = Behaviour k m (remark o x)
