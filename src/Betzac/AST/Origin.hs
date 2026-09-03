{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Betzac.AST.Origin (Origin (..), HasOrigin (..)) where

import Betzac.AST.Generic (Walk (..))
import Data.Data (Data)
import GHC.Generics (Generic)

-- | Why a node is in the tree, once the desugarer has stated what was left implicit.
data Origin
    = Written
    | Implied
    | Restated
    | Contradicted
    -- ^ Written, but asked for alongside something no leg can satisfy at once.
    -- A leg holding one of these permits nothing (cf. the empty moveset).
    deriving (Eq, Show, Data, Generic)

instance Walk Origin where
    walk _ _ acc = acc

class HasOrigin a where
    origin :: a -> Origin

instance HasOrigin () where -- Stripped phase, for uniformity
    origin () = Written
