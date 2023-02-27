{-# LANGUAGE TupleSections #-}

module Domain.StrictnessResults (
  StrictnessResults (..),
  ArgumentStrictness (..),
  makeResults,
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

makeResults :: AnalysisResults Strictness -> StrictnessResults
makeResults (AnalysisResults res) = StrictnessResults $ fmap (second makeResult) res

makeResult :: FunctionSemantics VariableIdentifier Strictness -> ArgumentStrictness
makeResult (FunctionSemantics args sem) = ArgumentStrictness $ fmap (makeArg args sem) args
 where
  makeArg vars sem it = (it,) $ sem (fromList undefined $ (it, Strict) : ((,Lazy) <$> filter (/= it) vars))
