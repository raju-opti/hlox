module Ast where
import Token
data Expression =
  Binary Expression TokenWithContext Expression
  | Unary TokenWithContext Expression
  | Literal TokenWithContext
  | Grouping Expression
  | IdentifierExpr TokenWithContext
  | Assignment TokenWithContext Expression
  deriving (Show, Eq)

data Statement =
  ExpressionStatement Expression
  | PrintStatement Expression
  | Declaration TokenWithContext (Maybe Expression)
  deriving (Show, Eq)
