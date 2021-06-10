module Ch16.FunctorSpec where

import Ch16.ArbitraryTestInstances ()
import Ch16.Functor
import Ch16.FunctorLaws
import Test.Hspec
import Test.QuickCheck
import Utils (runQuickCheckWithHSpec)

main :: IO ()
main = hspec $ do
  describe "Functor Or" $ do
    it "should map second value" $ do
      fmap (+ 1) (Second 1) `shouldBe` (Second 2 :: Or String Int)
    it "should do nothing with first value" $ do
      fmap (+ 1) (First "Test") `shouldBe` (First "Test" :: Or String Int)
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Or String Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Or Int Int -> Bool)

  describe "Functor Identity" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Identity String -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Identity Int -> Bool)

  describe "Functor Pair" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Pair String -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Pair Int -> Bool)

  describe "Functor Two" $ do
    it "should modify the last value" $ do
      fmap (+ 3) (Two "Test" 2) `shouldBe` Two "Test" 5
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Two String Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Two Int Int -> Bool)

  describe "Functor Three" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Three String String Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Three String String Int -> Bool)

  describe "Functor Three'" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Three' String Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Three' String Int -> Bool)

  describe "Functor Four" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Four String String String Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Four String String String Int -> Bool)

  describe "Functor Four'" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Four' String Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Four' String Int -> Bool)

  describe "Functor BoolAndSomethingElse" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: BoolAndSomethingElse String -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> BoolAndSomethingElse Int -> Bool)

  describe "Functor BoolAndMaybeSomethingElse" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: BoolAndMaybeSomethingElse String -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> BoolAndMaybeSomethingElse Int -> Bool)

  describe "Functor Company" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Company String String Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Company String String Int -> Bool)

  describe "Functor More" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: More String Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> More String Int -> Bool)

  describe "Functor Quant" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Quant String Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Quant String Int -> Bool)

  describe "Functor K" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: K String Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> K String Int -> Bool)

  describe "Functor EvilGoateeConst" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: EvilGoateeConst String Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> EvilGoateeConst String Int -> Bool)

  describe "Functor LiftItOut" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: LiftItOut Pair Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> LiftItOut Pair Int -> Bool)

  describe "Functor Parappa" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Parappa Pair Pair Int -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Parappa Pair Pair Int -> Bool)

  describe "Functor IgnoreOne" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: IgnoreOne Pair Pair Int String -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> IgnoreOne Pair Pair String Int -> Bool)

  describe "Functor Notorious" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: Notorious Pair String Int String -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> Notorious Pair String Int Int -> Bool)

  describe "Functor List" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: List String -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> List Int -> Bool)

  describe "Functor GoatLord" $ do
    it "should check identity law" $ do
      runQuickCheckWithHSpec (functorIdentityLaw :: GoatLord String -> Bool)
    it "should check composition law" $ do
      runQuickCheckWithHSpec (functorCompositionLaw :: Fun Int String -> Fun String Int -> GoatLord Int -> Bool)
