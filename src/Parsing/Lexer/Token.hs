module Parsing.Lexer.Token (Token (..)) where

data Token
  = Literal Integer
  | Name String
  | Plus
  | Dash
  | Star
  | Slash
  | If
  | Then
  | Else
  | OpenParen
  | CloseParen
  | Comma
  | Equal
  deriving (Show, Eq)
