module Data.Poset
  ( Poset (..),
  )
where

import Prelude hiding ((<=))

class Eq a => Poset a where
  (<=) :: a -> a -> Bool
