module Util (
  (.:.),
  combinations,
)
where

import Control.Monad (replicateM)

(.:.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:.) = (.) . (.)

combinations :: Int -> [a] -> [[a]]
combinations = replicateM
