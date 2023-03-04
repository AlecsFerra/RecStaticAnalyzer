{-# LANGUAGE StandaloneDeriving #-}

module Domain.Sign (Sign (..), signValueSemantics) where

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
import Environment (Environment, insert)
import Language.Syntax (Expression (Variable), VariableIdentifier)

data Sign
    = Bottom
    | NonZero
    | EqualZero
    | GreaterZero
    | GreaterEqZero
    | LowerZero
    | LowerEqZero
    | Top

deriving instance Show Sign

deriving instance Eq Sign

instance Poset Sign where
    Bottom <= _ = True
    _ <= Bottom = False
    _ <= Top = True
    Top <= _ = False
    EqualZero <= LowerEqZero = True
    EqualZero <= GreaterEqZero = True
    EqualZero <= EqualZero = True
    EqualZero <= _ = False
    _ <= EqualZero = False
    LowerZero <= LowerEqZero = True
    LowerZero <= NonZero = True
    LowerZero <= LowerZero = True
    LowerZero <= _ = False
    _ <= LowerZero = False
    GreaterZero <= GreaterEqZero = True
    GreaterZero <= NonZero = True
    GreaterZero <= GreaterZero = True
    GreaterZero <= _ = False
    _ <= GreaterZero = False
    x <= y = x == y

instance MeetSemiLattice Sign where
    Bottom /\ _ = Bottom
    _ /\ Bottom = Bottom
    Top /\ x = x
    x /\ Top = x
    NonZero /\ NonZero = NonZero
    EqualZero /\ EqualZero = EqualZero
    GreaterZero /\ GreaterZero = GreaterZero
    GreaterEqZero /\ GreaterEqZero = GreaterEqZero
    LowerZero /\ LowerZero = LowerZero
    LowerEqZero /\ LowerEqZero = LowerEqZero
    NonZero /\ LowerZero = LowerZero
    NonZero /\ GreaterZero = GreaterZero
    NonZero /\ LowerEqZero = LowerZero
    NonZero /\ GreaterEqZero = GreaterZero
    LowerEqZero /\ LowerZero = LowerZero
    LowerEqZero /\ EqualZero = EqualZero
    LowerEqZero /\ NonZero = LowerZero
    LowerEqZero /\ GreaterEqZero = EqualZero
    GreaterEqZero /\ GreaterZero = GreaterZero
    GreaterEqZero /\ EqualZero = EqualZero
    GreaterEqZero /\ NonZero = GreaterZero
    GreaterEqZero /\ LowerEqZero = EqualZero
    EqualZero /\ LowerEqZero = EqualZero
    EqualZero /\ GreaterEqZero = EqualZero
    LowerZero /\ LowerEqZero = LowerZero
    LowerZero /\ NonZero = LowerZero
    GreaterZero /\ GreaterEqZero = GreaterZero
    GreaterZero /\ NonZero = GreaterZero
    _ /\ _ = Bottom

instance BoundedMeetSemiLattice Sign where
    top = Top

instance JoinSemiLattice Sign where
    Top \/ _ = Top
    _ \/ Top = Top
    Bottom \/ x = x
    x \/ Bottom = x
    NonZero \/ NonZero = NonZero
    EqualZero \/ EqualZero = EqualZero
    GreaterZero \/ GreaterZero = GreaterZero
    GreaterEqZero \/ GreaterEqZero = GreaterEqZero
    LowerZero \/ LowerZero = LowerZero
    LowerEqZero \/ LowerEqZero = LowerEqZero
    NonZero \/ LowerZero = NonZero
    NonZero \/ GreaterZero = NonZero
    LowerEqZero \/ LowerZero = LowerEqZero
    LowerEqZero \/ EqualZero = LowerEqZero
    GreaterEqZero \/ GreaterZero = GreaterEqZero
    GreaterEqZero \/ EqualZero = GreaterEqZero
    EqualZero \/ LowerEqZero = LowerEqZero
    EqualZero \/ GreaterEqZero = GreaterEqZero
    EqualZero \/ LowerZero = LowerEqZero
    EqualZero \/ GreaterZero = GreaterEqZero
    LowerZero \/ LowerEqZero = LowerEqZero
    LowerZero \/ NonZero = NonZero
    LowerZero \/ EqualZero = LowerEqZero
    LowerZero \/ GreaterZero = NonZero
    GreaterZero \/ GreaterEqZero = GreaterEqZero
    GreaterZero \/ NonZero = NonZero
    GreaterZero \/ EqualZero = GreaterEqZero
    GreaterZero \/ LowerZero = NonZero
    _ \/ _ = Top

instance BoundedJoinSemiLattice Sign where
    bottom = Bottom

instance Lattice Sign

instance BoundedLattice Sign

instance Finite Sign where
    all =
        [ Bottom
        , NonZero
        , EqualZero
        , GreaterZero
        , GreaterEqZero
        , LowerZero
        , LowerEqZero
        , Top
        ]

signValueSemantics :: ValueSemantics Sign
signValueSemantics =
    ValueSemantics
        { literal = literal'
        , (*#) = mul
        , (+#) = add
        , cond = cond'
        }

literal' :: Integer -> Sign
literal' x
    | x == 0 = EqualZero
    | x > 0 = GreaterZero
    | otherwise = LowerZero

add :: Sign -> Sign -> Sign
add _ Bottom = Bottom
add Bottom _ = Bottom
add Top _ = Top
add _ Top = Top
add LowerZero LowerZero = LowerZero
add LowerZero EqualZero = LowerZero
add LowerZero LowerEqZero = LowerZero
add EqualZero LowerZero = LowerZero
add EqualZero EqualZero = EqualZero
add EqualZero GreaterZero = GreaterZero
add EqualZero LowerEqZero = LowerEqZero
add EqualZero NonZero = NonZero
add EqualZero GreaterEqZero = GreaterEqZero
add GreaterZero EqualZero = GreaterZero
add GreaterZero GreaterZero = GreaterZero
add GreaterZero GreaterEqZero = GreaterZero
add LowerEqZero LowerZero = LowerZero
add LowerEqZero EqualZero = LowerEqZero
add LowerEqZero LowerEqZero = LowerEqZero
add NonZero EqualZero = NonZero
add GreaterEqZero EqualZero = GreaterEqZero
add GreaterEqZero GreaterZero = GreaterZero
add GreaterEqZero GreaterEqZero = GreaterEqZero
add _ _ = Top

mul :: Sign -> Sign -> Sign
mul _ Bottom = Bottom
mul Bottom _ = Bottom
mul EqualZero _ = EqualZero
mul _ EqualZero = EqualZero
mul LowerZero LowerZero = GreaterZero
mul LowerZero GreaterZero = LowerZero
mul LowerZero LowerEqZero = LowerEqZero
mul LowerZero NonZero = NonZero
mul LowerZero GreaterEqZero = LowerEqZero
mul GreaterZero LowerZero = LowerZero
mul GreaterZero GreaterZero = GreaterZero
mul GreaterZero LowerEqZero = LowerEqZero
mul GreaterZero NonZero = NonZero
mul GreaterZero GreaterEqZero = GreaterEqZero
mul LowerEqZero LowerZero = LowerEqZero
mul LowerEqZero GreaterZero = LowerEqZero
mul LowerEqZero LowerEqZero = GreaterEqZero
mul LowerEqZero GreaterEqZero = LowerEqZero
mul NonZero LowerZero = NonZero
mul NonZero GreaterZero = NonZero
mul NonZero NonZero = NonZero
mul GreaterEqZero LowerZero = LowerEqZero
mul GreaterEqZero GreaterZero = GreaterEqZero
mul GreaterEqZero LowerEqZero = LowerEqZero
mul GreaterEqZero GreaterEqZero = GreaterEqZero
mul _ _ = Top

type Env = Environment VariableIdentifier Sign
cond' :: Sign -> Expression -> Env -> (Sign, Sign, Env, Env)
cond' EqualZero _ env = (top, bottom, env, env)
cond' NonZero _ env = (bottom, top, env, env)
cond' GreaterZero _ env = (bottom, top, env, env)
cond' LowerZero _ env = (bottom, top, env, env)
cond' sign (Variable id) env = (top, top, insert id EqualZero env, insert id (sign /\ NonZero) env)
cond' _ _ env = (top, top, env, env)
