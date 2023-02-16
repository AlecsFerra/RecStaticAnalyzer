module Util (
  (.:.),
  combinations,
  note,
  guard',
)
where

import Control.Monad (replicateM)

(.:.) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:.) = (.) . (.)

combinations :: Int -> [a] -> [[a]]
combinations = replicateM

note :: a -> Maybe b -> Either a b
note a Nothing = Left a
note _ (Just b) = Right b

guard' :: Bool -> a -> Either a ()
guard' False = Left
guard' True = const $ Right ()
