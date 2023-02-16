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

import Data.Map qualified as M (Map, delete, empty, fromList, insert, keys, lookup)
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup, (*), (<=))

data Environment k v = Environment
    { onEmpty :: k -> v
    , env :: M.Map k v
    }

instance (Eq k, Eq v) => Eq (Environment k v) where
    Environment _ l == Environment _ r = l == r

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
