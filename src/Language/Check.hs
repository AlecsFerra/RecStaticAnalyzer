module Language.Check (checkProgram, CheckError (..)) where

import Control.Monad (foldM)
import Data.Foldable (forM_)
import Environment (Environment, fromDefault, insert, lookup)
import Language.Syntax (Expression (..), FunctionDefinition (..), FunctionIdentifier (..), Program (Program), VariableIdentifier (..))
import Util (guard', note)
import Prelude hiding (lookup)

type Arity = Int

type FEnv = Environment FunctionIdentifier (Maybe Arity)

type Env = Environment VariableIdentifier Bool

data CheckError
  = DuplicateFunctionIdentifier FunctionIdentifier
  | UnknownFunction FunctionIdentifier
  | WrongArity FunctionIdentifier Arity Arity
  | UnknownVariable VariableIdentifier
  | DuplicateParameter VariableIdentifier
  | DuplicateConstant VariableIdentifier
  deriving (Show)

checkProgram :: Program -> Either CheckError ()
checkProgram (Program definitions) = do
  fenv <- foldM buildFenv (fromDefault $ const Nothing) definitions
  forM_ definitions (checkFunction fenv)

checkFunction :: FEnv -> FunctionDefinition -> Either CheckError ()
checkFunction fenv (FunctionDefinition _ args body) = do
  env <- checkArgs args
  checkExpression fenv env body
 where
  checkArgs = foldM comb (fromDefault $ const False)
  comb env id | lookup id env = Left $ DuplicateParameter id
  comb env id = Right $ insert id True env

buildFenv :: FEnv -> FunctionDefinition -> Either CheckError FEnv
buildFenv fenv (FunctionDefinition id args _) = do
  unusedName fenv id
  pure $ insert id (Just $ length args) fenv
 where
  unusedName fenv id = case lookup id fenv of
    Nothing -> pure ()
    Just _ -> Left $ DuplicateFunctionIdentifier id

checkExpression :: FEnv -> Env -> Expression -> Either CheckError ()
checkExpression fenv env = checkExpression'
 where
  checkExpression' (Variable id) | lookup id env = pure ()
  checkExpression' (Variable id) = Left $ UnknownVariable id
  checkExpression' (Literal _) = pure ()
  checkExpression' (Conditional guard thenBranch elseBranch) =
    checkExpression' guard
      *> checkExpression' thenBranch
      *> checkExpression' elseBranch
  checkExpression' (Addition l r) = checkExpression' l *> checkExpression' r
  checkExpression' (Multiplication l r) = checkExpression' l *> checkExpression' r
  checkExpression' (Application id args) = do
    forM_ args checkExpression'
    arity <- note (UnknownFunction id) $ lookup id fenv
    guard' (arity == length args) $ WrongArity id arity $ length args

-- >>> checkProgram (Program [FunctionDefinition (FunctionIdentifier "s") [VariableIdentifier "x"] (Conditional (Variable (VariableIdentifier "x")) (Literal 0) (Application (FunctionIdentifier "f") [Variable (VariableIdentifier "x"),Addition (Literal 0) (Multiplication (Literal (-1)) (Variable (VariableIdentifier "x")))])),FunctionDefinition (FunctionIdentifier "f") [VariableIdentifier "x",VariableIdentifier "y"] (Conditional (Variable (VariableIdentifier "x")) (Literal 1) (Conditional (Variable (VariableIdentifier "y")) (Multiplication (Literal (-1)) (Literal 1)) (Application (FunctionIdentifier "f") [Addition (Variable (VariableIdentifier "x")) (Multiplication (Literal (-1)) (Variable (VariableIdentifier "l"))),Addition (Variable (VariableIdentifier "y")) (Multiplication (Literal (-1)) (Literal 1))])))] (Addition (Literal 1) (Literal 2)))
-- Left (UnknownVariable (VariableIdentifier "l"))
