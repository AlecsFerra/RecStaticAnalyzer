module Analysis.StrictnessResults (
    StrictnessResults (..),
    ArgumentStrictness (..),
) where

import Domain.Strictness (Strictness (..))
import Language.Syntax (FunctionIdentifier (..), VariableIdentifier (..))

newtype StrictnessResults = StrictnessResults [(FunctionIdentifier, ArgumentStrictness)]
    deriving (Show)

newtype ArgumentStrictness = ArgumentStrictness [(VariableIdentifier, Strictness)]
    deriving (Show)
