module Analysis.StrictnessResults (StrictnessResults (..), pretty) where

import Data.List (intercalate)
import Domain.Strictness (Strictness (..))
import Language.Syntax (FunctionIdentifier (..), VariableIdentifier (..))
import Text.Printf (printf)

newtype StrictnessResults = StrictnessResults [(FunctionIdentifier, ArgumentStrictness)]
    deriving (Show)

type ArgumentStrictness = [(VariableIdentifier, Strictness)]

pretty :: StrictnessResults -> String
pretty (StrictnessResults defs) = intercalate "\n" $ fmap prettyDefinition defs

prettyDefinition :: (FunctionIdentifier, ArgumentStrictness) -> String
prettyDefinition (id, args) = printf "%s(%s)" (prettyFunctionId id) (prettyArgs args)

prettyArgs :: ArgumentStrictness -> String
prettyArgs args = intercalate ", " $ fmap prettyArg args

prettyArg :: (VariableIdentifier, Strictness) -> String
prettyArg (id, s) = printf "%s %s" (prettyStrictness s) (prettyVariableId id)

prettyFunctionId :: FunctionIdentifier -> String
prettyFunctionId (FunctionIdentifier id) = id

prettyVariableId :: VariableIdentifier -> String
prettyVariableId (VariableIdentifier id) = id

prettyStrictness :: Strictness -> String
prettyStrictness Strict = "S"
prettyStrictness Lazy = "L"
