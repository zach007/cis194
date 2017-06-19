import Test.Hspec
import Lib
import Week1

main :: IO ()
main = hspec $ do
  describe "How to wirte test" $ do
    it "Should be able to run test" $ do
      someString `shouldBe` "someString"

  describe "toDigit" $ do
      it "should return a list of digit of a integer" $ do
        toDigit 123 `shouldBe` [1,2,3]

  describe "toDigitRev" $ do
      it "should return a list of digit of a integer in a reverse order" $ do
        toDigitRev 123 `shouldBe` [3,2,1]
