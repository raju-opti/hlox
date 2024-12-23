module Interpreter where
import Ast
import Token
import Control.Exception
import qualified Data.HashTable.IO as H


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

unaryOp:: TokenWithContext -> LoxValue -> IO LoxValue
unaryOp (TokenWithContext Minus _ _) (LoxNumber n) = return $ LoxNumber (-n)
unaryOp token@(TokenWithContext Minus _ _) _ = throw $ operandNotNumber token
unaryOp (TokenWithContext Bang _ _) value =  return $ LoxBool (not (isTruthy value))
unaryOp _ _ = throw $ RuntimeError Nothing "Failed to evaluate unary operation"

binaryOp :: TokenWithContext -> LoxValue -> LoxValue -> IO LoxValue
binaryOp (TokenWithContext Plus _ _) (LoxNumber l) (LoxNumber r) = return $ LoxNumber (l + r)
binaryOp (TokenWithContext Plus _ _) (LoxString l) (LoxString r) = return $ LoxString (l ++ r)
binaryOp token@(TokenWithContext Plus _ _) _ _ = throw $ RuntimeError (Just token) "Operands must be two numbers or two strings."

binaryOp (TokenWithContext Minus _ _) (LoxNumber l) (LoxNumber r) = return $ LoxNumber (l - r)
binaryOp token@(TokenWithContext Minus _ _) _ _ = throw $ bothOperandsNotNumber token

binaryOp (TokenWithContext Slash _ _) (LoxNumber l) (LoxNumber r) = return $ LoxNumber (l / r)
binaryOp token@(TokenWithContext Slash _ _) _ _ = throw $ bothOperandsNotNumber token

binaryOp (TokenWithContext Star _ _) (LoxNumber l) (LoxNumber r) = return $ LoxNumber (l * r)
binaryOp token@(TokenWithContext Star _ _) _ _ = throw $ bothOperandsNotNumber token

binaryOp (TokenWithContext Greater _ _) (LoxNumber l) (LoxNumber r) = return $ LoxBool (l > r)
binaryOp token@(TokenWithContext Greater _ _) _ _ = throw $ bothOperandsNotNumber token

binaryOp (TokenWithContext GreaterEqual _ _) (LoxNumber l) (LoxNumber r) = return $ LoxBool (l >= r)
binaryOp token@(TokenWithContext GreaterEqual _ _) _ _ = throw $ bothOperandsNotNumber token

binaryOp (TokenWithContext Less _ _) (LoxNumber l) (LoxNumber r) = return $ LoxBool (l < r)
binaryOp token@(TokenWithContext Less _ _) _ _ = throw $ bothOperandsNotNumber token

binaryOp (TokenWithContext LessEqual _ _) (LoxNumber l) (LoxNumber r) = return $ LoxBool (l <= r)
binaryOp token@(TokenWithContext LessEqual _ _) _ _ = throw $ bothOperandsNotNumber token

-- FixMe: Haskell value comparison is not the same as Lox value comparison
binaryOp (TokenWithContext EqualEqual _ _) l r = return $ LoxBool (l == r)
binaryOp (TokenWithContext BangEqual _ _) l r = return $ LoxBool (l /= r)

binaryOp _ _ _ = throw $ RuntimeError Nothing "Failed to evaluate binary operation"


type HashTable k v = H.BasicHashTable k v

data Environment = Environment {
  envParent :: Maybe Environment,
  envValues :: IO (HashTable String LoxValue)
}

newEnvironment :: Maybe Environment -> Environment
newEnvironment parent = Environment parent H.new

getVar ::Environment -> String -> IO (Maybe LoxValue)
getVar env name = do
  values <- envValues env
  val <- H.lookup values name
  case val of
    Just _ -> return val
    Nothing -> case envParent env of
      Just parent -> getVar parent name
      Nothing -> return Nothing

setVar :: Environment -> String -> LoxValue -> IO ()
setVar env name value = do
  values <- envValues env
  H.insert values name value

hasVar :: Environment -> String -> IO Bool
hasVar env name = do
  value <- getVar env name
  case value of
    Just _ -> return True
    Nothing -> return False


evalExpression :: Environment -> Expression -> IO LoxValue
evalExpression _ (Literal (TokenWithContext (NumberToken n) _ _)) = return $ LoxNumber n
evalExpression _ (Literal (TokenWithContext (StringToken s) _ _)) = return $ LoxString s
evalExpression _ (Literal (TokenWithContext TrueToken _ _)) = return $ LoxBool True
evalExpression _ (Literal (TokenWithContext FalseToken _ _)) = return $ LoxBool False
evalExpression _ (Literal (TokenWithContext Nil _ _)) = return LoxNil

evalExpression env (Unary token expr) = evalExpression env expr >>= unaryOp token

evalExpression env (Binary left token right) = do
  leftValue <- evalExpression env left
  rightValue <- evalExpression env right
  binaryOp token leftValue rightValue

evalExpression env (Grouping expr) = evalExpression env expr
evalExpression _ _ = throw $ RuntimeError Nothing "Failed to evaluate expression"

globalEnv :: Environment
globalEnv = newEnvironment Nothing

evalStatement :: Environment -> Statement -> IO ()
evalStatement env (ExpressionStatement expr) = do
  evalExpression env expr
  return ()

evalStatement env (PrintStatement expr) = do
  val <- evalExpression env expr
  print val

eval:: [Statement] -> IO ()
eval statements = do
  mapM_ (evalStatement globalEnv) statements
