module Ch15.FirstSpec where

import Ch15.First
import Ch15.Laws
import Ch15.Optional
import Test.Hspec
import Test.QuickCheck
import Utils (runQuickCheckWithHSpec)

generateOnly :: Arbitrary a => Gen (Optional a)
generateOnly = Only <$> arbitrary

generateNada :: Gen (Optional a)
generateNada = return Nada

generateFirst' :: Arbitrary a => Gen (First' a)
generateFirst' = do
  optionalValue <- arbitrary
  return First' {getFirst' = optionalValue}

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = frequency [(1, generateNada), (1, generateOnly)]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = generateFirst'

main :: IO ()
main = hspec $ do
  describe "First Monoid Laws" $ do
    it "associativity law" $ do
      runQuickCheckWithHSpec (associativityLaw :: First' String -> First' String -> First' String -> Bool)
    it "left identity law" $ do
      runQuickCheckWithHSpec (leftIdentityLaw :: First' String -> Bool)
    it "right identity law" $ do
      runQuickCheckWithHSpec (rightIdentityLaw :: First' String -> Bool)
