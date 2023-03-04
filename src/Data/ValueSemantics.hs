module Data.ValueSemantics (ValueSemantics (..)) where

import Environment (Environment)
import Language.Syntax (Expression, VariableIdentifier)

type Env l = Environment VariableIdentifier l
data ValueSemantics l = ValueSemantics
    { literal :: Integer -> l
    , (*#) :: l -> l -> l
    , (+#) :: l -> l -> l
    , (/#) :: l -> l -> l
    , cond :: l -> Expression -> Env l -> (l, l, Env l, Env l)
    }
