module Parsing.Parser.Syntax (Program (..), Expression (..), Operator (..), Declaration (..)) where

data Operator = Addition | Subtraction | Multiplication | Division
  deriving (Show)

data Expression
  = IntegerLiteral Integer
  | Variable String
  | Application String [Expression]
  | BinaryOperator Operator Expression Expression
  | Negation Expression
  | Conditional Expression Expression Expression
  deriving (Show)

data Declaration = Declaration String [String] Expression
  deriving (Show)

newtype Program = Program [Declaration]
  deriving (Show)
