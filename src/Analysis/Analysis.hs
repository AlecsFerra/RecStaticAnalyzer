{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Analysis.Analysis (run, AnalysisResults (..)) where

import Analysis.FunctionSemantics (FunctionSemantics (..))
import Control.Arrow (Arrow (..))
import Data.Finite (Finite)
import Data.Lattice (
  BoundedLattice,
  BoundedJoinSemiLattice (..),
  JoinSemiLattice (..),
  MeetSemiLattice (..),
 )
import Data.ValueSemantics (ValueSemantics (..))
import Environment (Environment, fromList, keys, lookup)
import Language.Syntax (
  Expression (..),
  FunctionDefinition (..),
  FunctionIdentifier (..),
  Program (..),
  VariableIdentifier,
 )
import Prelude hiding (lookup)

type Env l =
  Environment VariableIdentifier l

type FEnv l =
  Environment FunctionIdentifier (Function l)

data Function l
  = Function
      (FunctionSemantics VariableIdentifier l)
      Expression
  deriving (Eq)

newtype AnalysisResults l
  = AnalysisResults
      [(FunctionIdentifier, FunctionSemantics VariableIdentifier l)]

run :: forall l. (BoundedLattice l, Finite l) => ValueSemantics l -> Program -> AnalysisResults l
run (ValueSemantics literal (*#) (+#)) (Program definitions) = results $ fix step bottomFenv
 where
  fix :: Eq e => (e -> e) -> e -> e
  fix f bottom = fst $ head $ filter (uncurry (==)) $ zip sequence $ tail sequence
   where
    sequence = iterate f bottom

  semantics :: FEnv l -> Expression -> Env l -> l
  semantics fenv e env = semantics' e
   where
    semantics' (Variable id) = lookup id env
    semantics' (Literal n) = literal n
    semantics' (Addition l r) = semantics' l +# semantics' r
    semantics' (Multiplication l r) = semantics' l *# semantics' r
    semantics' (Conditional guard thenClause elseClause) =
      semantics' guard /\ (semantics' thenClause \/ semantics' elseClause)
    semantics' (Application id args) =
      let args' = fmap semantics' args
          (Function (FunctionSemantics argNames functional) _) = lookup id fenv
          env' = fromList undefinedId $ zip argNames args'
       in functional env'

  step :: FEnv l -> FEnv l
  step fenv = fmap step' fenv
   where
    step' (Function (FunctionSemantics args _) body) = Function (FunctionSemantics args (semantics fenv body)) body

  bottomFenv :: FEnv l
  bottomFenv = fromList undefinedId $ fmap makeBottomDef definitions
   where
    makeBottomDef (FunctionDefinition name args body) = (name, Function (FunctionSemantics args (const bottom)) body)

undefinedId :: Show s => s -> a
undefinedId id = error $ "Reference to undefined identifier: " ++ show id ++ "."

results :: FEnv l -> AnalysisResults l
results fenv = AnalysisResults $ second clear . (id &&& (`lookup` fenv)) <$> keys fenv
 where
  clear (Function s _) = s
