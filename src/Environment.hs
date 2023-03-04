{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneDeriving #-}

module Environment (
    Environment,
    lookup,
    insert,
    delete,
    fromDefault,
    keys,
    fromList,
)
where

import Control.Arrow (Arrow (..))
import Data.Lattice (BoundedJoinSemiLattice (..), BoundedLattice, BoundedMeetSemiLattice (..), JoinSemiLattice (..), Lattice, MeetSemiLattice (..))
import Data.List (nub)
import Data.Map qualified as M (Map, delete, empty, fromList, insert, keys, lookup)
import Data.Maybe (fromMaybe)
import Data.Poset (Poset (..))
import Prelude hiding (lookup, (*), (<=))

data Environment k v = Environment
    { onEmpty :: k -> v
    , env :: M.Map k v
    }

deriving instance Functor (Environment k)

fromList :: (Ord k) => (k -> v) -> [(k, v)] -> Environment k v
fromList defaultValue bindings = Environment defaultValue (M.fromList bindings)

fromDefault :: (k -> v) -> Environment k v
fromDefault = flip Environment M.empty

lookup :: (Ord k) => k -> Environment k v -> v
lookup k e = fromMaybe (onEmpty e k) $ M.lookup k (env e)

insert :: (Ord k) => k -> v -> Environment k v -> Environment k v
insert k v e = e{env = M.insert k v (env e)}

delete :: (Ord k) => k -> Environment k v -> Environment k v
delete k e = e{env = M.delete k $ env e}

keys :: Environment k v -> [k]
keys = M.keys . env

instance (Eq k, Eq v) => Eq (Environment k v) where
    Environment _ e1 == Environment _ e2 = e1 == e2

instance (Ord k, Poset v) => Poset (Environment k v) where
    e1 <= e2 = all (uncurry (<=)) $ lookupAll (e1, e2)

instance (Ord k, MeetSemiLattice v) => MeetSemiLattice (Environment k v) where
    e1 /\ e2 = allWith e1 e2 (/\)

instance (Ord k, BoundedMeetSemiLattice v) => BoundedMeetSemiLattice (Environment k v) where
    top = fromDefault (const top)

instance (Ord k, JoinSemiLattice v) => JoinSemiLattice (Environment k v) where
    e1 \/ e2 = allWith e1 e2 (\/)

instance (Ord k, BoundedJoinSemiLattice v) => BoundedJoinSemiLattice (Environment k v) where
    bottom = fromDefault (const bottom)

instance (Ord k, Lattice v) => Lattice (Environment k v)

instance (Ord k, BoundedLattice v) => BoundedLattice (Environment k v)

keysIn :: (Eq k) => (Environment k v, Environment k v) -> [k]
keysIn = nub . uncurry (++) . (keys *** keys)

lookupAll :: Ord k => (Environment k v, Environment k v) -> [(v, v)]
lookupAll (e1, e2) = (flip lookup e1 &&& flip lookup e2) <$> keysIn (e1, e2)

allWith :: Ord k => Environment k a -> Environment k a -> (a -> a -> v) -> Environment k v
allWith e1 e2 (**) = fromList onEmpty' $ uncurry zip $ second (fmap $ uncurry (**)) $ (keysIn &&& lookupAll) (e1, e2)
  where
    onEmpty' = uncurry (**) . (onEmpty e1 &&& onEmpty e2)
