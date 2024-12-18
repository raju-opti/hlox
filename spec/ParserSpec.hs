module ParserSpec where

import SpecHelper
import Token
import Lexer
import Ast
import Data.Either 

scanResult :: String -> [TokenWithContext]
scanResult = fromRight [] . scan

spec :: Spec
spec = describe "Parser" $ do
  it "parses expressions" $ do
    parseExpression (scanResult "1") `shouldBe` (Right (Literal (TokenWithContext (NumberToken 1) 1 1)), [])
    parseExpression  (scanResult "(1)") `shouldBe` (Right (Grouping (Literal (TokenWithContext (NumberToken 1) 1 2))), [])
