module Interpreter where
import Ast
import Token
import Control.Exception
import Control.Monad (when)
import qualified Data.HashTable.IO as H
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time
import Data.HashTable.IO (new)


data LoxValue = LoxNil
              | LoxBool Bool
              | LoxNumber Double
              | LoxString String
              | LoxCallable Callable

data Callable = Callable {
  cArity :: Int,
  cCall :: [LoxValue] -> IO LoxValue
}

instance Show LoxValue where
  show LoxNil = "nil"
  show (LoxBool True) = "true"
  show (LoxBool False) = "false"
  show (LoxNumber n) = show n
  show (LoxString s) = s
  show (LoxCallable _) = "<callable>"

instance Eq LoxValue where
  LoxNil == LoxNil = True
  (LoxBool l) == (LoxBool r) = l == r
  (LoxNumber l) == (LoxNumber r) = l == r
  (LoxString l) == (LoxString r) = l == r
  _ == _ = False

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
  envValues :: HashTable String LoxValue
}

newEnvironment :: Maybe Environment -> IO Environment
newEnvironment parent = do
  Environment parent <$> H.new

getVar ::Environment -> String -> IO (Maybe LoxValue)
getVar env name = do
  val <- H.lookup (envValues env) name

  case val of
    Just _ -> return val
    Nothing -> case envParent env of
      Just parent -> getVar parent name
      Nothing -> return Nothing

defineVar :: Environment -> String -> LoxValue -> IO ()
defineVar env = H.insert (envValues env)

assignVar :: Environment -> String -> LoxValue -> IO ()
assignVar env name value = do
  existingValue <- H.lookup (envValues env) name
  case existingValue of
    Just _ -> H.insert (envValues env) name value
    Nothing -> case envParent env of
      Just parent -> assignVar parent name value
      Nothing -> throw $ RuntimeError Nothing ("Undefined variable '" ++ name ++ "'.")


evalExpression :: Environment -> Expression -> IO LoxValue
evalExpression _ (Literal (TokenWithContext (NumberToken n) _ _)) = return $ LoxNumber n
evalExpression _ (Literal (TokenWithContext (StringToken s) _ _)) = return $ LoxString s
evalExpression _ (Literal (TokenWithContext TrueToken _ _)) = return $ LoxBool True
evalExpression _ (Literal (TokenWithContext FalseToken _ _)) = return $ LoxBool False
evalExpression _ (Literal (TokenWithContext Nil _ _)) = return LoxNil

evalExpression env (IdentifierExpr token@(TokenWithContext (Identifier name) _ _)) = do
  value <- getVar env name
  case value of
    Just val -> return val
    Nothing -> throw $ RuntimeError (Just token) ("Undefined variable '" ++ name ++ "'.")
evalExpression env (Unary token expr) = evalExpression env expr >>= unaryOp token

evalExpression env (Assignment token@(TokenWithContext (Identifier name) _ _) expr) = do
  value <- evalExpression env expr
  catch (assignVar env name value >> return value) (\(RuntimeError _ message) -> throw $ RuntimeError (Just token) message)

evalExpression env (Binary left (TokenWithContext And _ _) right) = do
  leftValue <- evalExpression env left
  if isTruthy leftValue
    then evalExpression env right
    else return leftValue

evalExpression env (Binary left (TokenWithContext Or _ _) right) = do
  leftValue <- evalExpression env left
  if isTruthy leftValue
    then return leftValue
    else evalExpression env right

evalExpression env (Binary left token right) = do
  leftValue <- evalExpression env left
  rightValue <- evalExpression env right
  binaryOp token leftValue rightValue

evalExpression env (Grouping expr) = evalExpression env expr

evalExpression env (Call callee paren arguments) = do
  calleeValue <- evalExpression env callee
  case calleeValue of
    LoxCallable (Callable arity call) -> do
      when (length arguments /= arity) (throw $ RuntimeError (Just paren) "Expected number of arguments does not match.")
      argumentValues <- mapM (evalExpression env) arguments
      call argumentValues
    _ -> throw $ RuntimeError (Just paren) "Can only call functions and classes."


evalExpression _ _ = throw $ RuntimeError Nothing "Failed to evaluate expression"


evalStatement :: Environment -> Statement -> IO ()
evalStatement env (ExpressionStatement expr) = do
  evalExpression env expr
  return ()

evalStatement env (PrintStatement expr) = do
  val <- evalExpression env expr
  print val

evalStatement env (Declaration (TokenWithContext (Identifier name) _ _) val) = do
  case val of
    Just expr -> do
      value <- evalExpression env expr
      defineVar env name value
    Nothing -> defineVar env name LoxNil
  return ()

evalStatement env (Block statements) = do
  newEnv <- newEnvironment (Just env)
  mapM_ (evalStatement newEnv) statements


evalStatement env (IfStatement condition thenBranch elseBranch) = do
  conditionValue <- evalExpression env condition
  if isTruthy conditionValue
    then evalStatement env thenBranch
    else mapM_ (evalStatement env) elseBranch


evalStatement env stmt@(WhileStatement condition body) = do
  conditionValue <- evalExpression env condition
  when (isTruthy conditionValue) (evalStatement env body >> evalStatement env stmt)

evalStatement _ _ = throw $ RuntimeError Nothing "Failed to evaluate statement"

eval:: Environment -> [Statement] -> IO ()
eval env statements = do
  mapM_ (evalStatement env) statements


clock :: Callable
clock = Callable 0 $ \_ -> do
  LoxNumber . fromIntegral . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

globalEnv :: IO Environment
globalEnv = do
  env <- newEnvironment Nothing
  defineVar env "clock" (LoxCallable clock)
  return env
