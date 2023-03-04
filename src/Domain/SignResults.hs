{-# LANGUAGE TupleSections #-}

module Domain.SignResults (
    SignResults (..),
    makeSignResults,
) where

import Analysis.Analysis (AnalysisResults (..))
import Analysis.FunctionSemantics (FunctionSemantics (..))
import Data.Lattice (BoundedMeetSemiLattice (..))
import Domain.Sign (Sign)
import Environment (fromList)
import Language.Syntax (FunctionIdentifier, VariableIdentifier)

newtype SignResults = SignResults [(FunctionIdentifier, [VariableIdentifier], Sign)]

makeSignResults :: AnalysisResults Sign -> SignResults
makeSignResults (AnalysisResults res) = SignResults $ fmap makeResult res

makeResult :: (FunctionIdentifier, FunctionSemantics VariableIdentifier Sign) -> (FunctionIdentifier, [VariableIdentifier], Sign)
makeResult (fid, FunctionSemantics args sem) = (fid, args, sem $ fromList undefined $ fmap (,top) args)
