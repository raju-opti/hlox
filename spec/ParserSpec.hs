module ParserSpec where

import SpecHelper
import Token
import Lexer
import Ast
import Parser
import Data.Either


scanResult :: String -> [TokenWithContext]
scanResult = fromRight [] . scan

parseStatement :: [TokenWithContext] -> (Either ParserError Statement, [TokenWithContext])
parseStatement = runParse statementParser

parseDeclaration :: [TokenWithContext] -> (Either ParserError Statement, [TokenWithContext])
parseDeclaration = runParse declarationParser

spec :: Spec
spec = describe "Parser" $ do
  it "parses expressions" $ do
    parseExpression (scanResult "1") `shouldBe` (Right (Literal (TokenWithContext (NumberToken 1) 1 1)), [])
    parseExpression  (scanResult "(1)") `shouldBe` (Right (Grouping (Literal (TokenWithContext (NumberToken 1) 1 2))), [])
    parseExpression  (scanResult "-1") `shouldBe` (Right (Unary (TokenWithContext Minus 1 1) (Literal (TokenWithContext (NumberToken 1) 1 2))), [])
    parseExpression  (scanResult "!1") `shouldBe` (Right (Unary (TokenWithContext Bang 1 1) (Literal (TokenWithContext (NumberToken 1) 1 2))), [])

  it "parses declarations" $ do
    parseDeclaration (scanResult "var x = 1;") `shouldBe` (Right $ Declaration (TokenWithContext (Identifier "x") 1 5) (Just (Literal (TokenWithContext (NumberToken 1) 1 9))), [])
    parseDeclaration (scanResult "var x;") `shouldBe` (Right $ Declaration (TokenWithContext (Identifier "x") 1 5) Nothing, [])
    parseDeclaration (scanResult "var x = ;") `shouldBe` (Left $ ParserError (Just (TokenWithContext Semicolon 1 9)) "Expected expression", [TokenWithContext Semicolon 1 9])
    parseDeclaration (scanResult "var 1") `shouldBe` (Left $ ParserError (Just (TokenWithContext (NumberToken 1) 1 5)) "expect variable name", [TokenWithContext (NumberToken 1) 1 5])
 
  it "parses identifier expression" $ do
    parseExpression (scanResult "x") `shouldBe` (Right $ IdentifierExpr (TokenWithContext (Identifier "x") 1 1), [])

  it "parses expression statement" $ do
    parseStatement (scanResult "1;") `shouldBe` (Right $ ExpressionStatement (Literal (TokenWithContext (NumberToken 1) 1 1)), [])
  
  it "parses call expression" $ do
    parseExpression (scanResult "foo()") `shouldBe` 
      (Right $ Call 
        (IdentifierExpr (TokenWithContext (Identifier "foo") 1 1)) 
        (TokenWithContext RightParen 1 5) 
        [], 
      [])
      
    parseExpression (scanResult "foo(1, 2)") `shouldBe` 
      (Right $ Call 
        (IdentifierExpr (TokenWithContext (Identifier "foo") 1 1)) 
        (TokenWithContext RightParen 1 9) 
        [ Literal (TokenWithContext (NumberToken 1) 1 5)
        , Literal (TokenWithContext (NumberToken 2) 1 8)
        ], 
      [])
    parseExpression (scanResult "foo(1, 2)(3, 4)") `shouldBe` 
      (Right $ Call 
        (Call 
          (IdentifierExpr (TokenWithContext (Identifier "foo") 1 1)) 
          (TokenWithContext RightParen 1 9) 
          [ Literal (TokenWithContext (NumberToken 1) 1 5)
          , Literal (TokenWithContext (NumberToken 2) 1 8)
          ]) 
        (TokenWithContext RightParen 1 15) 
        [ Literal (TokenWithContext (NumberToken 3) 1 11)
        , Literal (TokenWithContext (NumberToken 4) 1 14)
        ], 
      [])