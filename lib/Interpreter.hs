module Interpreter where
import Ast
import Token
import Control.Exception

data LoxValue = LoxNil
              | LoxBool Bool
              | LoxNumber Double
              | LoxString String
              deriving (Eq)

instance Show LoxValue where
  show LoxNil = "nil"
  show (LoxBool True) = "true"
  show (LoxBool False) = "false"
  show (LoxNumber n) = show n
  show (LoxString s) = s



data RuntimeError = RuntimeError (Maybe TokenWithContext) String
  deriving (Eq)

instance Exception RuntimeError

instance Show RuntimeError where
  show (RuntimeError (Just (TokenWithContext _ line column)) message) = "Runtime Error: " ++ message ++ " at line " ++ show line ++ " column " ++ show column
  show (RuntimeError Nothing message) = "Runtime Error: " ++ message

isTruthy :: LoxValue -> Bool
isTruthy LoxNil = False
isTruthy (LoxBool False) = False
isTruthy _ = True

operandNotNumber :: TokenWithContext -> RuntimeError
operandNotNumber token = RuntimeError (Just token) "Operand must be a number."

bothOperandsNotNumber :: TokenWithContext -> RuntimeError
bothOperandsNotNumber token = RuntimeError (Just token) "Operands must be numbers."

unaryOp:: TokenWithContext -> LoxValue -> Either RuntimeError LoxValue
unaryOp (TokenWithContext Minus _ _) (LoxNumber n) = Right $ LoxNumber (-n)
unaryOp token@(TokenWithContext Minus _ _) _ = Left $ operandNotNumber token
unaryOp (TokenWithContext Bang _ _) value =  Right $ LoxBool (not (isTruthy value))
unaryOp _ _ = Left $ RuntimeError Nothing "Failed to evaluate unary operation"

binaryOp :: TokenWithContext -> LoxValue -> LoxValue -> Either RuntimeError LoxValue
binaryOp (TokenWithContext Plus _ _) (LoxNumber l) (LoxNumber r) = Right $ LoxNumber (l + r)
binaryOp (TokenWithContext Plus _ _) (LoxString l) (LoxString r) = Right $ LoxString (l ++ r)
binaryOp token@(TokenWithContext Plus _ _) _ _ = Left $ RuntimeError (Just token) "Operands must be two numbers or two strings."

binaryOp (TokenWithContext Minus _ _) (LoxNumber l) (LoxNumber r) = Right $ LoxNumber (l - r)
binaryOp token@(TokenWithContext Minus _ _) _ _ = Left $ bothOperandsNotNumber token

binaryOp (TokenWithContext Slash _ _) (LoxNumber l) (LoxNumber r) = Right $ LoxNumber (l / r)
binaryOp token@(TokenWithContext Slash _ _) _ _ = Left $ bothOperandsNotNumber token

binaryOp (TokenWithContext Star _ _) (LoxNumber l) (LoxNumber r) = Right $ LoxNumber (l * r)
binaryOp token@(TokenWithContext Star _ _) _ _ = Left $ bothOperandsNotNumber token

binaryOp (TokenWithContext Greater _ _) (LoxNumber l) (LoxNumber r) = Right $ LoxBool (l > r)
binaryOp token@(TokenWithContext Greater _ _) _ _ = Left $ bothOperandsNotNumber token

binaryOp (TokenWithContext GreaterEqual _ _) (LoxNumber l) (LoxNumber r) = Right $ LoxBool (l >= r)
binaryOp token@(TokenWithContext GreaterEqual _ _) _ _ = Left $ bothOperandsNotNumber token

binaryOp (TokenWithContext Less _ _) (LoxNumber l) (LoxNumber r) = Right $ LoxBool (l < r)
binaryOp token@(TokenWithContext Less _ _) _ _ = Left $ bothOperandsNotNumber token

binaryOp (TokenWithContext LessEqual _ _) (LoxNumber l) (LoxNumber r) = Right $ LoxBool (l <= r)
binaryOp token@(TokenWithContext LessEqual _ _) _ _ = Left $ bothOperandsNotNumber token

-- FixMe: Haskell value comparison is not the same as Lox value comparison
binaryOp (TokenWithContext EqualEqual _ _) l r = Right $ LoxBool (l == r)
binaryOp (TokenWithContext BangEqual _ _) l r = Right $ LoxBool (l /= r)

binaryOp _ _ _ = Left $ RuntimeError Nothing "Failed to evaluate binary operation"

evalExpression :: Expression -> Either RuntimeError LoxValue
evalExpression (Literal (TokenWithContext (NumberToken n) _ _)) = Right $ LoxNumber n
evalExpression (Literal (TokenWithContext (StringToken s) _ _)) = Right $ LoxString s
evalExpression (Literal (TokenWithContext TrueToken _ _)) = Right $ LoxBool True
evalExpression (Literal (TokenWithContext FalseToken _ _)) = Right $ LoxBool False
evalExpression (Literal (TokenWithContext Nil _ _)) = Right $ LoxNil

evalExpression (Unary token expr) = evalExpression expr >>= unaryOp token

evalExpression (Binary left token right) = do
  leftValue <- evalExpression left
  rightValue <- evalExpression right
  binaryOp token leftValue rightValue

evalExpression (Grouping expr) = evalExpression expr
evalExpression _ = Left $ RuntimeError Nothing "Failed to evaluate expression"

evalStatement :: Statement -> IO ()
evalStatement (ExpressionStatement expr) = do
  let val = evalExpression expr
  case val of
    Right _ -> return ()
    Left e -> throw e

evalStatement (PrintStatement expr) = do
  let val = evalExpression expr
  case val of
    Right v -> print v
    Left e -> throw e
