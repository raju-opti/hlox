module AnotherSpec where
import SpecHelper
-- import Lexer

spec :: Spec
spec = describe "Lexer" $ do
  it "has one equals one" $ do
    1 `shouldBe` 1
    -- (name) `shouldBe` "Lexer"

main :: IO ()
main = hspec spec
