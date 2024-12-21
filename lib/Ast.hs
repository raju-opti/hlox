module Ast where
import Token
data Expression =
  Binary Expression TokenWithContext Expression
  | Unary TokenWithContext Expression
  | Literal TokenWithContext
  | Grouping Expression
  deriving (Show, Eq)

data Statement =
  ExpressionStatement Expression
  | PrintStatement Expression
  deriving (Show, Eq)
