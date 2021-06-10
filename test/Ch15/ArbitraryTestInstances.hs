module Ch15.ArbitraryTestInstances where

import Ch15.Semigroup
import Test.QuickCheck

instance Arbitrary Trivial where
  arbitrary = return Trivial

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance Arbitrary BoolConj where
  arbitrary = BoolConj <$> arbitrary

instance Arbitrary BoolDisj where
  arbitrary = BoolDisj <$> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Fst a, Snd b]

--instance Arbitrary (Int -> Int) where
--  arbitrary = do
--    x <- chooseInt (-1000,1000)
--    return Combine $ \n -> Sum (n + x)
--
--combineAssociativityLaw ::
--  Combine Int Int ->
--  Combine Int Int ->
--  Combine Int Int ->
--  Int ->
--  Expectation
--combineAssociativityLaw f g h x = unCombine (f <> (g <> h)) x `shouldBe` unCombine ((f <> g) <> h) x
