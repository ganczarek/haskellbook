{-# LANGUAGE FlexibleInstances #-}

module Ch16.ArbitraryTestInstances where

import Ch16.Functor
import Ch16.Possibly
import Test.QuickCheck

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    return $ Three' a b1 b2

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    a3 <- arbitrary
    b <- arbitrary
    return $ Four' a1 a2 a3 b

instance (Arbitrary a) => Arbitrary (Possibly a) where
  arbitrary = do
    a <- arbitrary
    elements [LolNope, Yeppers a]

instance (Arbitrary a) => Arbitrary (BoolAndSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    elements [False' a, True' a]

instance (Arbitrary a) => Arbitrary (BoolAndMaybeSomethingElse a) where
  arbitrary = do
    a <- arbitrary
    elements [Falsish, Truish a]

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Company a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    elements [DeepBlue a b, Something c]

instance (Arbitrary a, Arbitrary b) => Arbitrary (More b a) where
  arbitrary = do
    a1 <- arbitrary
    a2 <- arbitrary
    b1 <- arbitrary
    b2 <- arbitrary
    elements [L a1 b1 a2, R b1 a1 b2]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Finance, Desk a, Bloor b]

instance (Arbitrary a) => Arbitrary (K a b) where
  arbitrary = K <$> arbitrary

instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = GoatyConst <$> arbitrary

instance (Arbitrary a) => Arbitrary (LiftItOut Pair a) where
  arbitrary = LiftItOut <$> arbitrary

instance (Arbitrary a) => Arbitrary (Parappa Pair Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ DaWrappa a b

instance (Arbitrary a, Arbitrary b) => Arbitrary (IgnoreOne Pair Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ IgnoringSomething a b

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Notorious Pair a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Notorious a b c

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Nil, Cons a b]

instance (Arbitrary a) => Arbitrary (GoatLord a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    elements [NoGoat, OneGoat a, MoreGoats b c d]
