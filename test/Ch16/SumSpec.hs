module Ch16.SumSpec where

import Ch16.FunctorLaws
import Ch16.Sum
import Test.Hspec
import Test.QuickCheck
import Utils (runQuickCheckWithHSpec)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

main :: IO ()
main = hspec $ do
  describe "Functor Sum" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Sum Int String -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Sum String Int -> Bool)
