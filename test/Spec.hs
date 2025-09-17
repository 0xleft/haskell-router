import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "This test shoudl not fail" $ do
    it "shoud not fail" $ do
      True `shouldBe` True