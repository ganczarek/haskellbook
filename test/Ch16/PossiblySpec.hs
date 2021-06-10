module Ch16.PossiblySpec where

import Ch16.ArbitraryTestInstances
import Ch16.Possibly
import Ch16.FunctorLaws
import Test.Hspec
import Test.QuickCheck
import Utils (runQuickCheckWithHSpec)

main :: IO ()
main = hspec $ do
  describe "Functor Possibly" $ do
    it "should ignore LolNope" $ do
      fmap (+ 1) LolNope `shouldBe` LolNope
    it "should map wrapped value" $ do
      fmap (++ "!") (Yeppers "Test") `shouldBe` Yeppers "Test!"
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Possibly String -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Possibly Int -> Bool)
