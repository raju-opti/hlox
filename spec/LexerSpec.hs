module LexerSpec where
import SpecHelper
import Lexer
import Token

spec :: Spec
spec = describe "Lexer" $ do
  it "parses single charater tokens" $ do
    scan "+" `shouldBe` Right [TokenWithContext Plus 1 1]
    scan ";" `shouldBe` Right [TokenWithContext Semicolon 1 1]
    scan "+-;" `shouldBe` Right [
        TokenWithContext Plus 1 1,
        TokenWithContext Minus 1 2,
        TokenWithContext Semicolon 1 3
      ]

  it "parses two character tokens" $ do
    scan "!=" `shouldBe` Right [TokenWithContext BangEqual 1 1]
    scan "==" `shouldBe` Right [TokenWithContext EqualEqual 1 1]
    scan "===" `shouldBe` Right [TokenWithContext EqualEqual 1 1, TokenWithContext Equal 1 3]
    scan "==;" `shouldBe` Right [TokenWithContext EqualEqual 1 1, TokenWithContext Semicolon 1 3]

  it "parses correctly with two character lookahead" $ do
    scan "!" `shouldBe` Right [TokenWithContext Bang 1 1]
    scan "=;" `shouldBe` Right [TokenWithContext Equal 1 1, TokenWithContext Semicolon 1 2]

  it "parses ignoring comments" $ do
    scan "// this is a comment;" `shouldBe` Right []
    scan "///;" `shouldBe` Right []
    scan "// this is a comment;\n" `shouldBe` Right []
    scan "// this is a comment\n;" `shouldBe` Right [TokenWithContext Semicolon 2 1]
    scan "// this is a comment\n// another comment\n;" `shouldBe` Right [TokenWithContext Semicolon 3 1]
    scan "// this is a comment\n// another comment\n;+" `shouldBe` Right [TokenWithContext Semicolon 3 1, TokenWithContext Plus 3 2]
    scan "==// this is a comment\n// another comment\n;" `shouldBe` Right [TokenWithContext EqualEqual 1 1, TokenWithContext Semicolon 3 1]

  it "parses slash correctly" $ do
    scan "/" `shouldBe` Right [TokenWithContext Slash 1 1]
    scan "//" `shouldBe` Right []
    scan "//\n/" `shouldBe` Right [TokenWithContext Slash 2 1]

  it "parses string literals" $ do
    scan "\"hello\"" `shouldBe` Right [TokenWithContext (StringToken "hello") 1 1]
    scan "\"hello\";" `shouldBe` Right [
        TokenWithContext (StringToken "hello") 1 1,
        TokenWithContext Semicolon 1 8
      ]
    scan "\"hello\";\"world\"" `shouldBe` Right [
        TokenWithContext (StringToken "hello") 1 1,
        TokenWithContext Semicolon 1 8,
        TokenWithContext (StringToken "world") 1 9
      ]
    scan "\"hello\";\"world\";" `shouldBe` Right [
        TokenWithContext (StringToken "hello") 1 1,
        TokenWithContext Semicolon 1 8,
        TokenWithContext (StringToken "world") 1 9,
        TokenWithContext Semicolon 1 16
      ]
    scan "\"hello\nworld\";" `shouldBe` Right [
        TokenWithContext (StringToken "hello\nworld") 1 1,
        TokenWithContext Semicolon 2 7
      ]
    
    scan "\"hello" `shouldBe` Left [LexError 1 1 "unterminated string."]
    scan "\"hello\nworld" `shouldBe` Left [LexError 1 1 "unterminated string."]

  it "parses numbers" $ do
    scan "123" `shouldBe` Right [TokenWithContext (NumberToken 123) 1 1]
    scan "123;" `shouldBe` Right [
        TokenWithContext (NumberToken 123) 1 1,
        TokenWithContext Semicolon 1 4
      ]
    scan "123+456" `shouldBe` Right [
        TokenWithContext (NumberToken 123) 1 1,
        TokenWithContext Plus 1 4,
        TokenWithContext (NumberToken 456) 1 5
      ]

    scan "123.456" `shouldBe` Right [
        TokenWithContext (NumberToken 123.456) 1 1
      ]

    scan "123.+" `shouldBe` Right [
        TokenWithContext (NumberToken 123.0) 1 1,
        TokenWithContext Dot 1 4,
        TokenWithContext Plus 1 5
      ]

    scan ".123" `shouldBe` Right [
        TokenWithContext Dot 1 1,
        TokenWithContext (NumberToken 123) 1 2
      ]

    scan ".123+" `shouldBe` Right [
        TokenWithContext Dot 1 1,
        TokenWithContext (NumberToken 123) 1 2,
        TokenWithContext Plus 1 5
      ]

  it "parses identifiers" $ do
    scan "orchid" `shouldBe` Right [TokenWithContext (Identifier "orchid") 1 1]
    scan "or123" `shouldBe` Right [TokenWithContext (Identifier "or123") 1 1]
    scan "or_123" `shouldBe` Right [TokenWithContext (Identifier "or_123") 1 1]
    scan "_or" `shouldBe` Right [TokenWithContext (Identifier "_or") 1 1]
    scan "_" `shouldBe` Right [TokenWithContext (Identifier "_") 1 1]
    scan "orchid;" `shouldBe` Right [
        TokenWithContext (Identifier "orchid") 1 1,
        TokenWithContext Semicolon 1 7
      ]
    scan "orchid+android" `shouldBe` Right [
        TokenWithContext (Identifier "orchid") 1 1,
        TokenWithContext Plus 1 7,
        TokenWithContext (Identifier "android") 1 8
      ]

  it "parses keywords" $ do
    scan "and" `shouldBe` Right [TokenWithContext And 1 1]
    scan "and;" `shouldBe` Right [
        TokenWithContext And 1 1,
        TokenWithContext Semicolon 1 4
      ]
    scan "and+or" `shouldBe` Right [
        TokenWithContext And 1 1,
        TokenWithContext Plus 1 4,
        TokenWithContext Or 1 5
      ]

  it "parses multiple tokens" $ do
    scan "123+456;" `shouldBe` Right [
        TokenWithContext (NumberToken 123) 1 1,
        TokenWithContext Plus 1 4,
        TokenWithContext (NumberToken 456) 1 5,
        TokenWithContext Semicolon 1 8
      ]
    scan "123+456;orchid" `shouldBe` Right [
        TokenWithContext (NumberToken 123) 1 1,
        TokenWithContext Plus 1 4,
        TokenWithContext (NumberToken 456) 1 5,
        TokenWithContext Semicolon 1 8,
        TokenWithContext (Identifier "orchid") 1 9
      ]

  it "skips whitespace" $ do
    scan "  123  +  456  ;\r\t;" `shouldBe` Right [
        TokenWithContext (NumberToken 123) 1 3,
        TokenWithContext Plus 1 8,
        TokenWithContext (NumberToken 456) 1 11,
        TokenWithContext Semicolon 1 16,
        TokenWithContext Semicolon 1 19
      ]

  it "handles newlines" $ do
    scan "+\n   orchid" `shouldBe` Right [
        TokenWithContext Plus 1 1,
        TokenWithContext (Identifier "orchid") 2 4
      ]
    
    scan "+\n   orchid\n;" `shouldBe` Right [
        TokenWithContext Plus 1 1,
        TokenWithContext (Identifier "orchid") 2 4,
        TokenWithContext Semicolon 3 1
      ]

  it "returns error on unknown character" $ do
    scan "@" `shouldBe` Left [LexError 1 1 "unexpected character: @"]
    scan "+=@" `shouldBe` Left [LexError 1 3 "unexpected character: @"]
    scan "+=@-#" `shouldBe` Left [
        LexError 1 3 "unexpected character: @",
        LexError 1 5 "unexpected character: #"
      ]

  it "returns all encountered errors" $ do
    scan ";+@-#" `shouldBe` Left [
        LexError 1 3 "unexpected character: @",
        LexError 1 5 "unexpected character: #"
      ]
    scan ";+@-#;" `shouldBe` Left [
        LexError 1 3 "unexpected character: @",
        LexError 1 5 "unexpected character: #"
      ]
    scan ";+@-#;@\"abcd" `shouldBe` Left [
        LexError 1 3 "unexpected character: @",
        LexError 1 5 "unexpected character: #",
        LexError 1 7 "unexpected character: @",
        LexError 1 8 "unterminated string."
      ]
  
main :: IO ()
main = hspec spec
