module Domain.BoundedInterval (
    BoundedInterval,
    extensionalBounds,
    boundedIntervalValueSemantics,
) where

import Data.Finite (Finite (..))
import Data.Lattice (
    BoundedJoinSemiLattice (..),
    BoundedLattice,
    BoundedMeetSemiLattice (..),
    JoinSemiLattice (..),
    Lattice,
    MeetSemiLattice (..),
 )
import Data.Poset (Poset (..))
import Data.ValueSemantics (ValueSemantics (..))
import Environment (Environment)
import Language.Syntax (Expression, VariableIdentifier)
import Prelude hiding (div, (<=))

data Extended
    = NegativeInfinity
    | Value Integer
    | Infinity
    deriving (Ord)

data BoundedInterval
    = BoundedInterval Extended Extended
    | Bottom

deriving instance Eq Extended

deriving instance Eq BoundedInterval

instance Poset Extended where
    _ <= Infinity = True
    Value n <= Value m = m >= n
    NegativeInfinity <= _ = True
    _ <= _ = False

instance Poset BoundedInterval where
    _ <= Bottom = False
    Bottom <= _ = True
    BoundedInterval n m <= BoundedInterval n' m' = (n' <= n) && (m <= m')

instance JoinSemiLattice BoundedInterval where
    Bottom \/ x = x
    x \/ Bottom = x
    BoundedInterval n m \/ BoundedInterval n' m' = BoundedInterval (min n n') (min m m')

instance BoundedJoinSemiLattice BoundedInterval where
    bottom = Bottom

instance MeetSemiLattice BoundedInterval where
    Bottom /\ _ = Bottom
    _ /\ Bottom = Bottom
    BoundedInterval n m /\ BoundedInterval n' m'
        | max n n' <= min m m' = BoundedInterval (max n n') (min m m')
    _ /\ _ = Bottom

instance BoundedMeetSemiLattice BoundedInterval where
    top = BoundedInterval Infinity Infinity

instance Lattice BoundedInterval
instance BoundedLattice BoundedInterval

instance Finite BoundedInterval where
    all = (Bottom :) $ uncurry BoundedInterval <$> filter (uncurry (<=)) (bounds >>= (\x -> map (x,) bounds))
      where
        bounds =
            [NegativeInfinity]
                ++ fmap Value [fst extensionalBounds .. snd extensionalBounds]
                ++ [Infinity]

boundedIntervalValueSemantics :: (Integer, Integer) -> ValueSemantics BoundedInterval
boundedIntervalValueSemantics bounds =
    ValueSemantics
        { literal = literal' bounds
        , (*#) = mul bounds
        , (+#) = add bounds
        , (/#) = div bounds
        , cond = cond'
        }

literal' :: (Integer, Integer) -> Integer -> BoundedInterval
literal' (lower, upper) n
    | n < lower = BoundedInterval NegativeInfinity NegativeInfinity
    | n > upper = BoundedInterval Infinity Infinity
    | otherwise = BoundedInterval (Value n) (Value n)

mul :: (Integer, Integer) -> BoundedInterval -> BoundedInterval -> BoundedInterval
mul _ Bottom _ = Bottom
mul _ _ Bottom = Bottom
mul (lowerBound, upperBound) (BoundedInterval a b) (BoundedInterval c d) =
    BoundedInterval lower upper
  where
    lower = if lower' < Value lowerBound then NegativeInfinity else lower'
    upper = if upper' > Value upperBound then Infinity else upper'

    lower' = minimum [ac, ad, bc, bd]
    upper' = maximum [ac, ad, bc, bd]

    ac = mul' a c
    ad = mul' a d
    bc = mul' b c
    bd = mul' b d

    mul' Infinity NegativeInfinity = NegativeInfinity
    mul' NegativeInfinity Infinity = NegativeInfinity
    mul' Infinity Infinity = Infinity
    mul' NegativeInfinity NegativeInfinity = Infinity
    mul' Infinity (Value 0) = Value 0 -- Debatable
    mul' (Value 0) Infinity = Value 0
    mul' Infinity (Value x)
        | x > 0 = Infinity
        | otherwise = NegativeInfinity
    mul' (Value x) Infinity
        | x > 0 = Infinity
        | otherwise = NegativeInfinity
    mul' NegativeInfinity (Value 0) = Value 0 -- Debatable
    mul' (Value 0) NegativeInfinity = Value 0
    mul' NegativeInfinity (Value x)
        | x > 0 = NegativeInfinity
        | otherwise = Infinity
    mul' (Value x) NegativeInfinity
        | x > 0 = NegativeInfinity
        | otherwise = Infinity
    mul' (Value x) (Value y) = Value (x * y)

add :: (Integer, Integer) -> BoundedInterval -> BoundedInterval -> BoundedInterval
add _ Bottom _ = Bottom
add _ _ Bottom = Bottom
add (lowerBound, upperBound) (BoundedInterval a b) (BoundedInterval c d) =
    BoundedInterval lower upper
  where
    lower = if lower' < Value lowerBound then NegativeInfinity else lower'
    upper = if upper' > Value upperBound then Infinity else upper'

    lower' = add' a c
    upper' = add' b d

    add' Infinity _ = Infinity
    add' _ Infinity = Infinity
    add' NegativeInfinity _ = NegativeInfinity
    add' _ NegativeInfinity = NegativeInfinity
    add' (Value x) (Value y) = Value (x + y)

div :: (Integer, Integer) -> BoundedInterval -> BoundedInterval -> BoundedInterval
div = error "Unimplemented"

-- div _ _ (BoundedInterval (Value 0) (Value 0)) = Bottom
-- div

type Env = Environment VariableIdentifier BoundedInterval
cond' :: BoundedInterval -> Expression -> Env -> (BoundedInterval, BoundedInterval, Env, Env)
cond' = error "Unimplemented"

extensionalBounds :: (Integer, Integer)
extensionalBounds = (-10, 10)
