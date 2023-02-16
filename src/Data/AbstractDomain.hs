module Data.AbstractDomain (
  ShouldContinue (..),
  AbstractDomain (..),
)
where

import Data.Lattice (BoundedLattice)

data ShouldContinue a
  = Continue a
  | Stop a

class BoundedLattice a => AbstractDomain a where
  widen :: a -> a -> ShouldContinue a
  narrow :: a -> a -> ShouldContinue a
