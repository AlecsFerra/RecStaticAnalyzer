{-# LANGUAGE TupleSections #-}

module Domain.StrictnessResults (
  StrictnessResults (..),
  ArgumentStrictness (..),
  makeStrictnessResults,
) where

import Analysis.Analysis (AnalysisResults (AnalysisResults))
import Analysis.FunctionSemantics (FunctionSemantics (..))
import Control.Arrow (Arrow (..))
import Domain.Strictness (Strictness (..))
import Environment (fromList)
import Language.Syntax (FunctionIdentifier (..), VariableIdentifier (..))
import Prelude hiding (lookup)

newtype StrictnessResults = StrictnessResults [(FunctionIdentifier, ArgumentStrictness)]
  deriving (Show)

newtype ArgumentStrictness = ArgumentStrictness [(VariableIdentifier, Strictness)]
  deriving (Show)

makeStrictnessResults :: AnalysisResults Strictness -> StrictnessResults
makeStrictnessResults (AnalysisResults res) = StrictnessResults $ fmap (second makeResult) res

makeResult :: FunctionSemantics VariableIdentifier Strictness -> ArgumentStrictness
makeResult (FunctionSemantics args sem) = ArgumentStrictness $ fmap (makeArg args sem) args
 where
  makeArg vars sem it = (it,) $ sem (fromList undefined $ (it, Strict) : ((,Lazy) <$> filter (/= it) vars))
