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

-- run:: String -> IO ()
-- run input = do
--   let result = scanAndParseStatements input
--   case result of
--     Left err -> do
--       putStrLn err
--       exitWith (ExitFailure 65)
--     Right statements -> do
--       eval statements

repl':: IO ()
repl' = do
  putStr "λ> "
  line <- getLine
  let result = scanAndParseExpression line
  case result of
    Left err -> putStrLn err
    Right expr -> case evalExpression expr of
      Left err -> print err
      Right value -> print value
  repl'

repl:: IO ()
repl = do
  putStr "λ> "
  line <- getLine
  let result = scanAndParseStatements line
  case result of
    Left err -> putStrLn err
    Right statements -> do
      catch (eval statements) (print :: RuntimeError -> IO ())
  repl


runScript:: String -> IO ()
runScript file = do
  contents <- readFile file
  let result = scanAndParseStatements contents
  case result of
    Left err -> do
      putStrLn err
      exitWith (ExitFailure 65)
    Right statements -> do
      catch (eval statements) (\e -> do
        print (e :: RuntimeError)
        exitWith (ExitFailure 70))


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  if null args
    then repl
    else runScript (head args)
