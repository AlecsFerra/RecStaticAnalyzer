{-# LANGUAGE ImportQualifiedPost #-}

module Analysis.FunctionSemantics (FunctionSemantics (..)) where

import Control.Arrow (Arrow (..))
import Data.Finite qualified as F (Finite (..))
import Data.Lattice (
    BoundedJoinSemiLattice (..),
    BoundedLattice,
    BoundedMeetSemiLattice (..),
    JoinSemiLattice (..),
    Lattice,
    MeetSemiLattice (..),
 )
import Data.List (nub)
import Data.Poset (Poset (..))
import Environment (Environment, fromList)
import Util (combinations)
import Prelude hiding ((<=))

data FunctionSemantics k v
    = FunctionSemantics
        [k]
        (Environment k v -> v)

instance (F.Finite v, Eq v, Ord k) => Eq (FunctionSemantics k v) where
    f1 == f2 = sameK f1 f2 && allArgs f1 f2 (==)

instance (F.Finite v, Poset v, Ord k) => Poset (FunctionSemantics k v) where
    f1 <= f2 = sameK f1 f2 && allArgs f1 f2 (<=)

instance (F.Finite v, MeetSemiLattice v, Ord k) => MeetSemiLattice (FunctionSemantics k v) where
    f1 /\ f2 = combine f1 f2 (/\)

instance (F.Finite v, JoinSemiLattice v, Ord k) => JoinSemiLattice (FunctionSemantics k v) where
    f1 \/ f2 = combine f1 f2 (\/)

instance (F.Finite v, Lattice v, Ord k) => Lattice (FunctionSemantics k v)

instance (BoundedJoinSemiLattice v, Ord k, F.Finite v) => BoundedJoinSemiLattice (FunctionSemantics k v) where
    bottom = FunctionSemantics [] (const bottom)

instance (BoundedMeetSemiLattice v, Ord k, F.Finite v) => BoundedMeetSemiLattice (FunctionSemantics k v) where
    top = FunctionSemantics [] (const top)

instance (F.Finite v, BoundedLattice v, Ord k) => BoundedLattice (FunctionSemantics k v)

allArgs :: (Ord k, F.Finite v) => FunctionSemantics k v -> FunctionSemantics k v -> (v -> v -> Bool) -> Bool
allArgs (FunctionSemantics args1 sem1) (FunctionSemantics args2 sem2) (**) =
    all (uncurry (**) . (sem1 &&& sem2)) (fmap combine values)
  where
    args = nub $ args1 ++ args2
    values = combinations (length args) F.all
    combine value = fromList undefined (zip args value)

combine :: Eq k => FunctionSemantics k v -> FunctionSemantics k v -> (v -> v -> v) -> FunctionSemantics k v
combine (FunctionSemantics args1 sem1) (FunctionSemantics args2 sem2) (**) =
    FunctionSemantics (nub $ args1 ++ args2) (uncurry (**) . (sem1 &&& sem2))

sameK :: Eq k => FunctionSemantics k v -> FunctionSemantics k v -> Bool
sameK (FunctionSemantics k1 _) (FunctionSemantics k2 _) = k1 == k2
