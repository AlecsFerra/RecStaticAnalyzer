module Data.Lattice
  ( MeetSemiLattice (..),
    JoinSemiLattice (..),
    BoundedMeetSemiLattice (..),
    BoundedJoinSemiLattice (..),
    Lattice,
    BoundedLattice
  )
where

import Data.Poset (Poset)

class Poset a => MeetSemiLattice a where
  (/\) :: a -> a -> a

class MeetSemiLattice a => BoundedMeetSemiLattice a where
  bottom :: a

class Poset a => JoinSemiLattice a where
  (\/) :: a -> a -> a

class JoinSemiLattice a => BoundedJoinSemiLattice a where
  top :: a

class (MeetSemiLattice a, JoinSemiLattice a) => Lattice a

class (BoundedMeetSemiLattice a, BoundedJoinSemiLattice a) => BoundedLattice a
