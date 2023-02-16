module Data.AbstractSemantics (AbstractSemantics (..)) where

import Data.Lattice (Lattice)

class Lattice d => AbstractSemantics d where
  literal :: Integer -> d
  addition :: d -> d -> d
  multiplication :: d -> d -> d
  conditional :: d -> d -> d -> d
