{-# OPTIONS_GHC -Wno-partial-fields #-}

module Betzac.AST (
    BetzaProgram,
    QualifiedStmt (..),
    Directive (..),
    BetzaStmt (..),
    BetzaExpr (..),
    ChainExpr (..),
    OptionExpr (..),
    UnionExpr (..),
    ModifierExpr (..),
    ExponentExpr (..),
    AtomExpr (..),
    ChainOperator (..),
    Modifier (..),
    DirectionModifier (..),
    Direction (..),
    Behaviour (..),
    Exponent (..),
    ExponentKind (..),
    Label (..),
    Number,
)
where

import Data.List.NonEmpty (NonEmpty)

type BetzaProgram = [QualifiedStmt]

data QualifiedStmt
    = Override Directive
    | Plain Directive
    deriving (Eq, Show)

data Directive
    = Using FilePath
    | Export BetzaStmt
    | Bare BetzaStmt
    deriving (Eq, Show)

data BetzaStmt
    = Assign {label :: Label, expr :: BetzaExpr}
    | Alias {alias :: Label, label :: Label}
    | Resolve {label :: Label}
    | Anonymous {expr :: BetzaExpr}
    deriving (Eq, Show)

-- Strictly related to expressions

data BetzaExpr = BetzaExpr ChainExpr
    deriving (Eq, Show)

data ChainExpr = ChainExpr OptionExpr [(ChainOperator, OptionExpr)]
    deriving (Eq, Show)

data OptionExpr = Choose BetzaExpr | IffUnblocked BetzaExpr | Mandatory UnionExpr
    deriving (Eq, Show)

newtype UnionExpr = UnionExpr (NonEmpty ModifierExpr)
    deriving (Eq, Show)

data ModifierExpr = ModifierExpr {setup :: Bool, modifiers :: [Modifier], expExpr :: ExponentExpr}
    deriving (Eq, Show)

data ExponentExpr = ExponentExpr AtomExpr (Maybe Exponent)
    deriving (Eq, Show)

data AtomExpr = Paren BetzaExpr | From Label
    deriving (Eq, Show)

data ChainOperator = Step | Sequence
    deriving (Eq, Show)

data Modifier = Directional DirectionModifier | Behavioural Behaviour
    deriving (Eq, Show)

data DirectionModifier = Amalgamated Direction Direction | Single Direction
    deriving (Eq, Show)

data Direction = Forward | Backward | Leftward | Rightward | Sideway | Vertically | All
    deriving (Eq, Show)

data Behaviour = Capture | Leap | Initial | Jump | Move | NoJump | Hop | Any
    deriving (Eq, Show)

data Exponent = Exponent (Maybe ChainOperator) [Modifier] ExponentKind
    deriving (Eq, Show)

data ExponentKind = Infinite | Slippery | Repeat Number
    deriving (Eq, Show)

data Label = Upper Char | Descriptor String | Leaper Number Number
    deriving (Eq, Show)

type Number = Int
