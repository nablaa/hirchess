import Test.Hspec

main :: IO ()
main = hspec $
        describe "Example Spec" $
          it "should pass" $
            (0 :: Int) `shouldBe` (0 :: Int)
