module ParserSpec where

import SpecHelper
import Token
import Lexer
import Ast
import Parser
import Data.Either
import GHC.Conc (par)

scanResult :: String -> [TokenWithContext]
scanResult = fromRight [] . scan

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
