module Betzac.Debug.Strip (strip) where

import Betzac.AST
import qualified Data.List.NonEmpty as NE

strip :: BetzaProgram p -> BetzaProgram Stripped
strip = stripProgram

stripProgram :: BetzaProgram p -> BetzaProgram Stripped
stripProgram = map stripQualifiedStmt

stripQualifiedStmt :: QualifiedStmt p -> QualifiedStmt Stripped
stripQualifiedStmt (Override d _) = Override (stripDirective d) ()
stripQualifiedStmt (Plain d _) = Plain (stripDirective d) ()

stripDirective :: Directive p -> Directive Stripped
stripDirective (Using f _) = Using f ()
stripDirective (Export s _) = Export (stripStmt s) ()
stripDirective (Bare s _) = Export (stripStmt s) ()

stripStmt :: BetzaStmt p -> BetzaStmt Stripped
stripStmt (Assign l e _) = Assign (stripLabel l) (stripExpr e) ()
stripStmt (Anonymous e _) = Anonymous (stripExpr e) ()

stripExpr :: BetzaExpr p -> BetzaExpr Stripped
stripExpr (BetzaExpr c _) = BetzaExpr (stripChainExpr c) ()

stripChainExpr :: ChainExpr p -> ChainExpr Stripped
stripChainExpr (ChainExpr u mcl _) = ChainExpr (stripUnionExpr u) (maybe Nothing (Just . stripChainLeg) $ mcl) ()

stripChainLeg :: ChainLeg p -> ChainLeg Stripped
stripChainLeg (ChainLeg op c _) = ChainLeg (stripChainOperator op) (stripChainExpr c) ()

stripUnionExpr :: UnionExpr p -> UnionExpr Stripped
stripUnionExpr (UnionExpr ms _) = UnionExpr (NE.map stripModifierExpr ms) ()

stripModifierExpr :: ModifierExpr p -> ModifierExpr Stripped
stripModifierExpr (ModifierExpr s ms e _) = ModifierExpr s (map stripModifier ms) (stripExponentExpr e) ()

stripExponentExpr :: ExponentExpr p -> ExponentExpr Stripped
stripExponentExpr (ExponentExpr a me _) = ExponentExpr (stripAtomExpr a) (maybe Nothing (Just . stripExponent) $ me) ()

stripAtomExpr :: AtomExpr p -> AtomExpr Stripped
stripAtomExpr (Paren e _) = Paren (stripExpr e) ()
stripAtomExpr (From l _) = From (stripLabel l) ()

stripChainOperator :: ChainOperator p -> ChainOperator Stripped
stripChainOperator (ChainOperator k m _) = ChainOperator (stripChainKind k) (stripChainModality m) ()

stripChainKind :: ChainKind p -> ChainKind Stripped
stripChainKind (Step _) = Step ()
stripChainKind (Sequence _) = Sequence ()

stripChainModality :: ChainModality p -> ChainModality Stripped
stripChainModality (Mandatory _) = Mandatory ()
stripChainModality (Choose _) = Choose ()
stripChainModality (IffUnblocked _) = IffUnblocked ()

stripModifier :: Modifier p -> Modifier Stripped
stripModifier (Directional d _) = Directional (stripDirectionModifier d) ()
stripModifier (Behavioural b _) = Behavioural (stripBehaviour b) ()

stripDirectionModifier :: DirectionModifier p -> DirectionModifier Stripped
stripDirectionModifier (Amalgamated x y _) = Amalgamated (stripDirection x) (stripDirection y) ()
stripDirectionModifier (Single d _) = Single (stripDirection d) ()

stripDirection :: Direction p -> Direction Stripped
stripDirection (Forward _) = Forward ()
stripDirection (Backward _) = Backward ()
stripDirection (Leftward _) = Leftward ()
stripDirection (Rightward _) = Rightward ()
stripDirection (Sideway _) = Sideway ()
stripDirection (Vertically _) = Vertically ()
stripDirection (All _) = All ()

stripBehaviour :: Behaviour p -> Behaviour Stripped
stripBehaviour (Behaviour k m _) = Behaviour (stripBehaviourKind k) (stripBehaviourModality m) ()

stripBehaviourKind :: BehaviourKind p -> BehaviourKind Stripped
stripBehaviourKind (Capture _) = Capture ()
stripBehaviourKind (Leap _) = Leap ()
stripBehaviourKind (Initial _) = Initial ()
stripBehaviourKind (Jump _) = Jump ()
stripBehaviourKind (Move _) = Move ()
stripBehaviourKind (NoJump _) = NoJump ()
stripBehaviourKind (Hop _) = Hop ()

stripBehaviourModality :: BehaviourModality p -> BehaviourModality Stripped
stripBehaviourModality (Once _) = Once ()
stripBehaviourModality (Twice _) = Twice ()
stripBehaviourModality (Any _) = Any ()

stripExponent :: Exponent p -> Exponent Stripped
stripExponent (Exponent mco ms k _) =
    Exponent
        (maybe Nothing (Just . stripChainOperator) $ mco)
        (map stripModifier ms)
        (stripExponentKind k)
        ()

stripExponentKind :: ExponentKind p -> ExponentKind Stripped
stripExponentKind (Infinite _) = Infinite ()
stripExponentKind (Slippery _) = Slippery ()
stripExponentKind (Repeat n _) = Repeat n ()

stripLabel :: Label p -> Label Stripped
stripLabel (Upper c _) = Upper c ()
stripLabel (Descriptor s _) = Descriptor s ()
stripLabel (Leaper x y _) = Leaper x y ()
