module Data.ValueSemantics (ValueSemantics (..)) where

data ValueSemantics l = ValueSemantics
    { literal :: Integer -> l
    , (*#) :: l -> l -> l
    , (+#) :: l -> l -> l
    }
