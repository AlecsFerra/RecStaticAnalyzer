{-# LANGUAGE StandaloneDeriving #-}

module Domain.Strictness (
  Strictness (..),
  strictnessValueSemantics,
)
where

import Data.Finite (Finite (..))
import Data.Lattice (
  BoundedJoinSemiLattice (..),
  BoundedLattice,
  BoundedMeetSemiLattice (..),
  JoinSemiLattice (..),
  Lattice,
  MeetSemiLattice (..),
 )
import Data.Poset (Poset (..))
import Data.ValueSemantics (ValueSemantics (..))

-- | Two point latice describing the strictness
data Strictness
  = -- | It may be strict but we cannot be sure so we over approximate his lazy-ness (May terminate) [1]
    Lazy
  | -- | It is for sure strict (It will always not terminate) [0]
    Strict

instance Show Strictness where
  show Lazy = "L <1>"
  show Strict = "S <0>"

deriving instance Eq Strictness

instance Poset Strictness where
  Lazy <= Strict = False
  _ <= _ = True

instance MeetSemiLattice Strictness where
  Lazy /\ Lazy = Lazy
  _ /\ _ = Strict

instance BoundedMeetSemiLattice Strictness where
  bottom = Strict

instance JoinSemiLattice Strictness where
  Strict \/ Strict = Strict
  _ \/ _ = Lazy

instance BoundedJoinSemiLattice Strictness where
  top = Lazy

instance Lattice Strictness

instance BoundedLattice Strictness

instance Finite Strictness where
  all = [Lazy, Strict]

strictnessValueSemantics :: ValueSemantics Strictness
strictnessValueSemantics =
  ValueSemantics
    { literal = const top
    , (*#) = (/\)
    , (+#) = (/\)
    }
