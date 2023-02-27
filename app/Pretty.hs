{-# LANGUAGE TypeApplications #-}

module Pretty (
  prettyStrictnessResult,
  prettyCheckError,
  prettyParseError,
) where

import Domain.StrictnessResults (ArgumentStrictness (..), StrictnessResults (..))
import Data.List (intercalate)
import Domain.Strictness (Strictness (..))
import Language.Check (CheckError (..))
import Language.Syntax (FunctionIdentifier (..), VariableIdentifier (..))
import Parsing.Parser (ParseError (..))
import Text.Printf (printf)

prettyStrictnessResult :: StrictnessResults -> String
prettyStrictnessResult (StrictnessResults defs) = intercalate "\n" $ fmap prettyDefinition defs

prettyDefinition :: (FunctionIdentifier, ArgumentStrictness) -> String
prettyDefinition (id, args) = printf "%s(%s)" (prettyFunctionId id) (prettyArgs args)

prettyArgs :: ArgumentStrictness -> String
prettyArgs (ArgumentStrictness args) = intercalate ", " $ fmap prettyArg args

prettyArg :: (VariableIdentifier, Strictness) -> String
prettyArg (id, s) = printf "%s %s" (prettyStrictness s) (prettyVariableId id)

prettyFunctionId :: FunctionIdentifier -> String
prettyFunctionId (FunctionIdentifier id) = id

prettyVariableId :: VariableIdentifier -> String
prettyVariableId (VariableIdentifier id) = id

prettyStrictness :: Strictness -> String
prettyStrictness Strict = "S"
prettyStrictness Lazy = "L"

prettyParseError :: String -> ParseError -> String
prettyParseError fileName error = printf "Error while parsing '%s': %s" fileName $ pretty error
 where
  pretty (UnexpectedCharacter c) = printf "Unexpected '%c'" c
  pretty (UnexpectedToken t) = printf "Unexpected '%s'" t
  pretty UnexpectedEOF = "Unexpected end of file"

prettyCheckError :: String -> CheckError -> String
prettyCheckError fileName error = printf @(String -> String -> String) "Error while checking '%s': %s" fileName $ pretty error
 where
  pretty (DuplicateFunctionIdentifier (FunctionIdentifier id)) = printf "function %s is declared multiple times" id
  pretty (UnknownFunction (FunctionIdentifier id)) = printf "unknown function %s" id
  pretty (WrongArity (FunctionIdentifier id) expected got) = printf "function %s expected %d arguments but got %d" id expected got
  pretty (UnknownVariable (VariableIdentifier id)) = printf "unknown variable %s" id
  pretty (DuplicateParameter (VariableIdentifier id)) = printf "parameter %s is declared multiple times" id
  pretty (DuplicateConstant (VariableIdentifier id)) = printf "duplicate constant %s declared" id
