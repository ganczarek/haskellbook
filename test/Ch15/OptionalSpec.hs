module Ch15.OptionalSpec where

import Ch15.Optional
import Data.Monoid
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Optional Monoid" $ do
    it "combines monoids inside Optional" $ do
      Only (1 :: Sum Int) <> Only 5 `shouldBe` Only 6
      
    it "combines with an empty Optional (left and right identity)" $ do
      (Nada :: Optional (Sum Int)) <> Only 5 `shouldBe` Only 5
      Only (5 :: Sum Int) <> Nada `shouldBe` Only 5
      
    it "combines empty Optionals" $ do
      (Nada :: Optional (Sum Int)) <> Nada `shouldBe` Nada
