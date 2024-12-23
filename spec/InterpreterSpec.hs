module InterpreterSpec where

import SpecHelper
import Token
import Lexer
import Ast
import Parser
import Interpreter
    ( RuntimeError(RuntimeError),
      LoxValue(LoxBool, LoxNumber, LoxString),
      evalExpression )
import Data.Either 

scanResult :: String -> [TokenWithContext]
scanResult = fromRight [] . scan

scanAndParse :: String -> Expression
scanAndParse = fromRight (Literal (TokenWithContext (NumberToken 0) 0 0)) . fst . parseExpression . scanResult

spec :: Spec
spec = describe "interpreter" $ do
  it "interprets expressions" $ do
    1 `shouldBe` 1
    -- evalExpression (scanAndParse "1") `shouldBe` Right (LoxNumber 1)
    -- evalExpression (scanAndParse "1 + 2") `shouldBe` Right (LoxNumber 3)
    -- evalExpression (scanAndParse "1 + 2 / 2") `shouldBe` Right (LoxNumber 2)
    -- evalExpression (scanAndParse "1 + 2 * 2") `shouldBe` Right (LoxNumber 5)
    -- evalExpression (scanAndParse "\"fo\" + \"o\"") `shouldBe` Right (LoxString "foo")
    -- evalExpression (scanAndParse "1 + true") `shouldBe` Left (RuntimeError (Just (TokenWithContext (Plus) 1 3)) "Operands must be two numbers or two strings.")
    -- evalExpression (scanAndParse "!true") `shouldBe` Right (LoxBool False)
    -- evalExpression (scanAndParse "1 == 1") `shouldBe` Right (LoxBool True)
    -- evalExpression (scanAndParse "1 > 2") `shouldBe` Right (LoxBool False)    
