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

  describe "doubleEveryOther" $ do
      it "should double every number not the neibehood from the right" $ do
        doubleEveryOther [8,7,6,5] `shouldBe` [16,7,12,5]
        doubleEveryOther [1,2,3]   `shouldBe` [1,4,3]

  describe "sumDigit" $ do
      it "should return sum value of each number in a list " $ do
        sumDigit [16,7,12,5]  `shouldBe` 22
