module Language.Syntax (
  Expression (..),
  FunctionDefinition (..),
  Program (..),
  FunctionIdentifier (..),
  VariableIdentifier (..),
)
where

newtype VariableIdentifier = VariableIdentifier String
  deriving (Eq, Show, Ord)

newtype FunctionIdentifier = FunctionIdentifier String
  deriving (Eq, Show, Ord)

data Expression
  = Literal Integer
  | Variable VariableIdentifier
  | Addition Expression Expression
  | Multiplication Expression Expression
  | Division Expression Expression
  | Conditional Expression Expression Expression
  | Application FunctionIdentifier [Expression]
  deriving (Show, Eq)

data FunctionDefinition = FunctionDefinition FunctionIdentifier [VariableIdentifier] Expression
  deriving (Show)

newtype Program = Program [FunctionDefinition]
  deriving (Show)
