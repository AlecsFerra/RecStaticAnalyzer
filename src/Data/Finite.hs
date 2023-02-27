module Data.Finite (Finite (..)) where

class Finite f where
    all :: [f]
