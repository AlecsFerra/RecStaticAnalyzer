{-# LANGUAGE TupleSections #-}

module Analysis.Naive (eval) where

import Analysis.StrictnessResults (ArgumentStrictness (..), StrictnessResults (StrictnessResults))
import Data.Lattice (BoundedJoinSemiLattice (top), BoundedMeetSemiLattice (bottom), (/\), (\/))
import Domain.Strictness (Strictness (..))
import Environment (Environment, fromList, keys, lookup)
import Language.Syntax (Expression (..), FunctionDefinition (..), FunctionIdentifier, Program (Program), VariableIdentifier)
import Util (combinations)
import Prelude hiding (lookup)

type FEnv = Environment FunctionIdentifier Function

type Env = Environment VariableIdentifier Strictness

type FSemantics = Env -> Strictness

data Function
  = Function
      [VariableIdentifier]
      FSemantics
      Expression

undefinedId :: Show e => e -> a
undefinedId = error . ("Error: undefined identifier: " ++) . show

eval :: Program -> StrictnessResults
eval (Program definitions) = makeResults $ fix step $ makeBottom definitions

makeResults :: FEnv -> StrictnessResults
makeResults = StrictnessResults . makeResults'

makeResults' :: FEnv -> [(FunctionIdentifier, ArgumentStrictness)]
makeResults' fenv = makeFnResult <$> keys fenv
 where
  makeFnResult id = (id, makeArgs $ lookup id fenv)

makeArgs :: Function -> ArgumentStrictness
makeArgs (Function vars sem _) = ArgumentStrictness $ fmap (makeArg vars sem) vars
 where
  makeArg vars sem it = (it,) $ sem (fromList undefinedId $ (it, Strict) : ((,Lazy) <$> filter (/= it) vars))

isFixed :: (FEnv, FEnv) -> Bool
isFixed (env1, env2) = all isFixed' $ (\k -> (lookup k env1, lookup k env2)) <$> keys env1

isFixed' :: (Function, Function) -> Bool
isFixed' (Function v1 s1 _, Function _ s2 _) = all same envs
 where
  comb = combinations (length v1) [Strict, Lazy]
  envs = fmap (fromList undefinedId . zip v1) comb
  same env = s1 env == s2 env

fix :: (FEnv -> FEnv) -> FEnv -> FEnv
fix step bottom = fst $ head $ filter isFixed $ zip sequence (tail sequence)
 where
  sequence = iterate step bottom

-- s e = trace ("ITER:\n" ++ pretty (makeResults e) ++ "\n") e

makeBottom :: [FunctionDefinition] -> FEnv
makeBottom d = fromList undefinedId $ fmap makeBottom' d
 where
  makeBottom' (FunctionDefinition id args body) = (id, Function args (const bottom) body)

step :: FEnv -> FEnv
step fenv = fmap step' fenv
 where
  step' (Function args _ body) = Function args (semantics fenv body) body

semantics :: FEnv -> Expression -> Env -> Strictness
semantics fenv e env = semantics' e
 where
  semantics' (Variable id) = lookup id env
  semantics' (Literal _) = top
  semantics' (Addition l r) = semantics' l /\ semantics' r
  semantics' (Multiplication l r) = semantics' l /\ semantics' r
  semantics' (Conditional guard thenClause elseClause) =
    semantics' guard /\ (semantics' thenClause \/ semantics' elseClause)
  semantics' (Application id args) =
    let Function arguments semantics _ = lookup id fenv
        args' = fromList undefinedId $ zip arguments $ fmap semantics' args
     in semantics args'
