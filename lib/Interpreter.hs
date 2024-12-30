{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Use lambda-case" #-}
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
import Data.Maybe (fromMaybe)
import Parser (identifierName, printStatementParser)
import Data.Map as Map


data LoxValue = LoxNil
              | LoxBool Bool
              | LoxNumber Double
              | LoxString String
              | LoxCallable Callable
              | LoxFunction RtFunction
              | LoxClass RtClass
              | LoxInstance ClassInstance

data Callable = Callable {
  cArity :: Int,
  cCall :: [LoxValue] -> IO LoxValue
}

data RtFunction = RtFunction {
  fFunction :: AstFunction,
  fClosure :: Environment,
  fIsInitializer :: Bool
}

funArity :: RtFunction -> Int
funArity (RtFunction (AstFunction _ params _) _ _) = length params

callableFunction :: RtFunction -> Callable
callableFunction (RtFunction (AstFunction _ params body) cl isInit) = Callable (length params) $ \args -> do
          newEnv <- newEnvironment (Just cl)
          mapM_ (uncurry (defineVar newEnv)) (zip (fmap tokenName params) args)
          value <- eval newEnv body
          if isInit
          then fromMaybe LoxNil <$> getVar cl "this" (Just 0) 
          else 
            case value of
            Just (ReturnValue val) -> return val
            _ -> return LoxNil

data RtClass = RtClass {
  lcName :: String,
  lcMethods :: Map String RtFunction
}

classArity:: RtClass -> Int
classArity cls = maybe 0 funArity (getMethod "init" cls)

data ClassInstance = ClassInstance {
  iClass :: RtClass,
  iFields :: HashTable String LoxValue
}


bind:: LoxValue -> RtFunction -> IO RtFunction
bind ins (RtFunction (AstFunction name params body) cl isInit) = do
  newEnv <- newEnvironment (Just cl)
  H.insert (envValues newEnv) "this" ins
  return $ RtFunction (AstFunction name params body) newEnv isInit

instantiate :: RtClass -> IO LoxValue
instantiate cls = LoxInstance . ClassInstance cls <$> H.new

getLoxInstance :: LoxValue -> ClassInstance
getLoxInstance (LoxInstance ins) = ins

getMethod:: String -> RtClass -> Maybe RtFunction
getMethod name cls = Map.lookup name (lcMethods cls)

callableClass :: RtClass -> Callable
callableClass c = Callable (classArity c) $ \args -> do
  ins <- instantiate c
  let initMethod = getMethod "init" c
  case initMethod of
    Just fn -> do
      call <- callableFunction <$> bind ins fn
      cCall call args
      return ins
    _ -> return ins

callable:: LoxValue -> Maybe Callable
callable (LoxCallable c) = Just c
callable (LoxFunction fn) = Just $ callableFunction fn
callable (LoxClass c) = Just $ callableClass c
callable _ = Nothing

instance Show LoxValue where
  show LoxNil = "nil"
  show (LoxBool True) = "true"
  show (LoxBool False) = "false"
  show (LoxNumber n) = show n
  show (LoxString s) = s
  show (LoxFunction (RtFunction (AstFunction (TokenWithContext (Identifier name) _ _) _ _) _ _)) = "<fn " ++ name ++ ">"
  show (LoxClass (RtClass name _) ) = "<class " ++ name ++ ">"
  show (LoxInstance (ClassInstance (RtClass name _) _)) = "<instance " ++ name ++ ">"
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
  show (RuntimeError (Just (TokenWithContext _ line column)) message) =
    "Runtime Error: " ++ message ++ " at line " ++ show line ++ " column " ++ show column
  show (RuntimeError Nothing message) = "Runtime Error: " ++ message

tokenName :: TokenWithContext -> String
tokenName (TokenWithContext (Identifier name) _ _) = name

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

getVar ::Environment -> String -> Maybe Int -> IO (Maybe LoxValue)
getVar env name (Just distance) = do
  case distance of
    0 -> H.lookup (envValues env) name
    _ -> case envParent env of
      Just parent -> getVar parent name (Just (distance - 1))
      Nothing -> return Nothing

getVar env name Nothing = do
  case envParent env of
    Just parent -> getVar parent name Nothing
    Nothing -> H.lookup (envValues env) name

defineVar :: Environment -> String -> LoxValue -> IO ()
defineVar env = H.insert (envValues env)

assignVar :: Environment -> String -> Maybe Int -> LoxValue -> IO ()
assignVar env name (Just distance) value = do
  case distance of
    0 -> H.insert (envValues env) name value
    _ -> case envParent env of
      Just parent -> assignVar parent name (Just (distance - 1)) value
      Nothing -> return ()

assignVar env name Nothing value = do
  case envParent env of
    Just parent -> assignVar parent name Nothing value
    Nothing -> H.insert (envValues env) name value


evalExpression :: Environment -> Expression -> IO LoxValue
evalExpression _ (Literal (TokenWithContext (NumberToken n) _ _)) = return $ LoxNumber n
evalExpression _ (Literal (TokenWithContext (StringToken s) _ _)) = return $ LoxString s
evalExpression _ (Literal (TokenWithContext TrueToken _ _)) = return $ LoxBool True
evalExpression _ (Literal (TokenWithContext FalseToken _ _)) = return $ LoxBool False
evalExpression _ (Literal (TokenWithContext Nil _ _)) = return LoxNil

evalExpression env (IdentifierExpr token@(TokenWithContext (Identifier name) _ _) d) = do
  value <- getVar env name d
  case value of
    Just val -> return val
    Nothing -> throw $ RuntimeError (Just token) ("Undefined variable '" ++ name ++ "'.")
evalExpression env (Unary token expr) = evalExpression env expr >>= unaryOp token

evalExpression env (Assignment (IdentifierExpr token@(TokenWithContext (Identifier name) _ _) d) expr) = do
  value <- evalExpression env expr
  catch (assignVar env name d value>> return value) (\(RuntimeError _ message) -> throw $ RuntimeError (Just token) message)

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
  let callableValue = callable calleeValue
  case callableValue of
    Just (Callable arity call) -> do
      when (length arguments /= arity) (throw $ RuntimeError (Just paren) "Expected number of arguments does not match.")
      argumentValues <- mapM (evalExpression env) arguments
      call argumentValues
    _ -> throw $ RuntimeError (Just paren) "Can only call functions and classes."

evalExpression env (Get obj name) = do
  objValue <- evalExpression env obj
  case objValue of
    LoxInstance (ClassInstance cl fields) -> do
      value <- H.lookup fields (tokenName name)
      case value of
        Just val -> return val
        Nothing -> case getMethod (tokenName name) cl of
          Just fn -> do
            boundFn <- bind objValue fn
            return $ LoxFunction boundFn
          Nothing -> throw $ RuntimeError (Just name) ("Undefined property '" ++ tokenName name ++ "'.")
    _ -> throw $ RuntimeError (Just name) "Only instances have properties."

evalExpression env (Set obj name value) = do
  objValue <- evalExpression env obj
  case objValue of
    LoxInstance (ClassInstance _ fields) -> do
      val <- evalExpression env value
      H.insert fields (tokenName name) val
      return val
    _ -> throw $ RuntimeError (Just name) "Only instances have fields."

evalExpression env (ThisExpr _ d) = do
  value <- getVar env "this" (Just d)
  case value of
    Just val -> return val

evalExpression _ _ = throw $ RuntimeError Nothing "Failed to evaluate expression"

newtype ReturnValue = ReturnValue LoxValue
  deriving (Show, Eq)

evalStatement :: Environment -> Statement -> IO (Maybe ReturnValue)
evalStatement env (ExpressionStatement expr) = do
  evalExpression env expr
  return Nothing

evalStatement env (PrintStatement expr) = do
  val <- evalExpression env expr
  print val
  return Nothing

evalStatement env (VarDeclaration (TokenWithContext (Identifier name) _ _) val) = do
  case val of
    Just expr -> do
      value <- evalExpression env expr
      defineVar env name value
    Nothing -> defineVar env name LoxNil
  return Nothing

evalStatement env (Block statements) = do
  newEnv <- newEnvironment (Just env)
  eval newEnv statements


evalStatement env (IfStatement condition thenBranch elseBranch) = do
  conditionValue <- evalExpression env condition
  if isTruthy conditionValue
    then evalStatement env thenBranch
    else do
      maybe (return Nothing) (evalStatement env) elseBranch


evalStatement env stmt@(WhileStatement condition body) = do
  conditionValue <- evalExpression env condition
  if isTruthy conditionValue
    then do
      result <- evalStatement env body
      case result of
        Just ret -> return (Just ret)
        Nothing -> evalStatement env stmt
    else return Nothing

evalStatement env (FunDeclaration fn@(AstFunction (TokenWithContext (Identifier name) _ _) _ _)) = do
  let loxFunction = LoxFunction $ RtFunction fn env False
  defineVar env name loxFunction
  return Nothing

evalStatement env (ClassDeclaration (AstClass (TokenWithContext (Identifier name) _ _) methods)) = do
  let fnMethods = fmap makeFn methods
        where makeFn fn@(AstFunction token _ _) = (tokenName token, RtFunction fn env (tokenName token == "init"))
      loxClass = LoxClass $ RtClass name (Map.fromList fnMethods)
  defineVar env name loxClass
  return Nothing

evalStatement env (ReturnStatement _ expr) = do
  val <- maybe (return LoxNil) (evalExpression env) expr
  return $ Just (ReturnValue val)

evalStatement _ _ = throw $ RuntimeError Nothing "Failed to evaluate statement"

eval:: Environment -> [Statement] -> IO (Maybe ReturnValue)
eval _ [] = return Nothing
eval env (statement:statements) = do
  val <- evalStatement env statement
  case val of
    Nothing -> eval env statements
    _ -> return val

clock :: Callable
clock = Callable 0 $ \_ -> do
  LoxNumber . fromIntegral . floor . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds <$> getCurrentTime

globalEnv :: IO Environment
globalEnv = do
  env <- newEnvironment Nothing
  defineVar env "clock" (LoxCallable clock)
  return env
