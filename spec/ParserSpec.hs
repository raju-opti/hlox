module ParserSpec where

import SpecHelper
import Token
import Lexer
import Ast
import Parser
import Data.Either 

scanResult :: String -> [TokenWithContext]
scanResult = fromRight [] . scan

spec :: Spec
spec = describe "Parser" $ do
  it "parses expressions" $ do
    parseExpression (scanResult "1") `shouldBe` (Right (Literal (TokenWithContext (NumberToken 1) 1 1)), [])
    parseExpression  (scanResult "(1)") `shouldBe` (Right (Grouping (Literal (TokenWithContext (NumberToken 1) 1 2))), [])
    parseExpression  (scanResult "-1") `shouldBe` (Right (Unary (TokenWithContext Minus 1 1) (Literal (TokenWithContext (NumberToken 1) 1 2))), [])
    parseExpression  (scanResult "!1") `shouldBe` (Right (Unary (TokenWithContext Bang 1 1) (Literal (TokenWithContext (NumberToken 1) 1 2))), [])
