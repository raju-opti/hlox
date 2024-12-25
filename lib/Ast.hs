module Ast where
import Token
import Data.Map as Map

data Expression =
  Binary Expression TokenWithContext Expression
  | Unary TokenWithContext Expression
  | Literal TokenWithContext
  | Grouping Expression
  | IdentifierExpr TokenWithContext (Maybe Int)
  | Assignment Expression Expression
  | Call Expression TokenWithContext [Expression]
  deriving (Show, Eq)

type ResolvedExpression = (Expression, Map String Int)

data AstFunction = AstFunction {
  name :: TokenWithContext,
  params :: [TokenWithContext],
  body :: [Statement]
} deriving (Show, Eq)

data Statement =
  ExpressionStatement Expression
  | PrintStatement Expression
  | VarDeclaration TokenWithContext (Maybe Expression)
  | FunDeclaration AstFunction
  | Block [Statement]
  | IfStatement Expression Statement (Maybe Statement)
  | WhileStatement Expression Statement
  | ReturnStatement TokenWithContext (Maybe Expression)
  deriving (Show, Eq)
