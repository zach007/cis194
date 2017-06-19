import Test.Hspec
import Lib
import Week1

main :: IO ()
main = hspec $ do
  describe "How to wirte test" $ do
    it "Should be able to run test" $ do
      someString `shouldBe` "someString"


  describe "How to wirte test" $ do
    it "Should be able to run test" $ do
      test2 `shouldBe` "SomeString!!!"
