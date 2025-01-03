module Main where
import System.Environment
import System.IO
import Token
import Lexer
import Parser
import Interpreter
import Data.Either
import Data.Bifunctor
import Ast
import Data.Bifunctor (Bifunctor(bimap))
import System.Exit (exitWith, ExitCode (ExitFailure))
import Control.Exception
import Control.Monad
import Resolver

scanAndParseExpression :: String -> Either String Expression
scanAndParseExpression inp = do
  tokens <- (first (unlines. fmap show) . scan) inp
  let (result, _) = parseExpression tokens
  first show result

scanAndParseStatements :: String -> Either String [Statement]
scanAndParseStatements inp = do
  tokens <- (first (unlines. fmap show) . scan') inp
  let result = parseProgram tokens
  first show result

resolveStatements :: [Statement] -> Either String [Statement]
resolveStatements statements = 
  let (stmt, err, _) = resolve statements (ResolutionState [] NoneF NoneC)
  in if null err
    then Right stmt
    else Left (unlines (fmap show err))


-- run:: String -> IO ()
-- run input = do
--   let result = scanAndParseStatements input
--   case result of
--     Left err -> do
--       putStrLn err
--       exitWith (ExitFailure 65)
--     Right statements -> do
--       eval statements

-- repl':: IO ()
-- repl' = do
--   putStr "λ> "
--   line <- getLine
--   let result = scanAndParseExpression line
--   case result of
--     Left err -> putStrLn err
--     Right expr -> case evalExpression expr of
--       Left err -> print err
--       Right value -> print value
--   repl'

replEnv :: Environment -> IO ()
replEnv env = do
  putStr "λ> "
  line <- getLine
  let result = scanAndParseStatements line >>= resolveStatements
  case result of
    Left err -> putStrLn err
    Right statements -> do
      catch (void (eval env statements)) (print :: RuntimeError -> IO ())
  replEnv env

repl:: IO ()
repl = do
  env <- globalEnv
  replEnv env
  -- putStr "λ> "
  -- line <- getLine
  -- let result = scanAndParseStatements line
  -- case result of
  --   Left err -> putStrLn err
  --   Right statements -> do
  --     foo <- getVar env "foo"
  --     print foo
  --     catch (eval env statements) (print :: RuntimeError -> IO ())
  -- repl


runScript:: String -> IO ()
runScript file = do
  contents <- readFile file
  let result = scanAndParseStatements contents >>= resolveStatements
  case result of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 65)
    Right statements -> do
      env <- globalEnv
      catch (void (eval env statements)) (print :: RuntimeError -> IO ())

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  if null args
    then repl
    else runScript (head args)
