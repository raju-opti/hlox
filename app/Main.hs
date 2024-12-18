module Main where
import System.Environment
import System.IO
import Token
import Lexer

repl:: IO ()
repl = do
  putStr "λ> " 
  line <- getLine
  putStrLn line
  repl

runScript:: String -> IO ()
runScript file = do
  putStrLn $ "Running script: " ++ file ++ ", Lexer module is: " ++ name 

foo = StringT "foo"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  if null args
    then repl
    else runScript (head args)
