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

scanAndParseExpression :: String -> Either String Expression
scanAndParseExpression inp = do
  tokens <- (first (unlines. fmap show) . scan) inp
  let (result, _) = parseExpression tokens
  first show result


repl:: IO ()
repl = do
  putStr "λ> "
  line <- getLine
  let result = scanAndParseExpression line
  case result of
    Left err -> putStrLn err
    Right expr -> case evalExpression expr of
      Left err -> print err
      Right value -> print value
  repl

runScript:: String -> IO ()
runScript file = do
  putStrLn $ "Running script: " ++ file


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  if null args
    then repl
    else runScript (head args)
