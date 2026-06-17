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
    Label (..),
    Number,
)
where

import Data.List.NonEmpty (NonEmpty)

type BetzaProgram = [QualifiedStmt]

data QualifiedStmt
    = Override Directive
    | Plain Directive
    deriving (Show)

data Directive
    = Using FilePath
    | Export BetzaStmt
    | Bare BetzaStmt
    deriving (Show)

data BetzaStmt
    = Assign {label :: Label, expr :: BetzaExpr}
    | Alias {alias :: Label, label :: Label}
    | Resolve {label :: Label}
    | Anonymous {expr :: BetzaExpr}
    deriving (Show)

-- Strictly related to expressions

data BetzaExpr = BetzaExpr ChainExpr
    deriving (Show)

data ChainExpr = ChainExpr OptionExpr [(ChainOperator, OptionExpr)]
    deriving (Show)

data OptionExpr = Choose BetzaExpr | IffUnblocked BetzaExpr | Mandatory UnionExpr
    deriving (Show)

newtype UnionExpr = UnionExpr (NonEmpty ModifierExpr)
    deriving (Show)

data ModifierExpr = ModifierExpr {setup :: Bool, modifiers :: [Modifier], atom :: ExponentExpr}
    deriving (Show)

data ExponentExpr = ExponentExpr AtomExpr (Maybe Exponent)
    deriving (Show)

data AtomExpr = Paren BetzaExpr | From Label
    deriving (Show)

data ChainOperator = Step | Sequence
    deriving (Show)

data Modifier = Directional DirectionModifier | Behavioural Behaviour
    deriving (Show)

data DirectionModifier = Amalgamated Direction Direction | Single Direction
    deriving (Show)

data Direction = Forward | Backward | Leftward | Rightward | Sideway | Vertically | All
    deriving (Show)

data Behaviour = Capture | Leap | Initial | Jump | Move | NoJump | Hop | Any
    deriving (Show)

data Exponent = Infinite | Repeat (Maybe ChainOperator) [Modifier] Number
    deriving (Show)

data Label = Upper Char | Descriptor String | Leaper Number Number
    deriving (Show)

type Number = Int
