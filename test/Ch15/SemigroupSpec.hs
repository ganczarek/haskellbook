{-# LANGUAGE FlexibleInstances #-}

module Ch15.SemigroupSpec where

import Ch15.ArbitraryTestInstances ()
import Ch15.Laws
import Ch15.Semigroup
import Data.Monoid
import Test.Hspec
import Utils (runQuickCheckWithHSpec)

main :: IO ()
main = hspec $ do
  describe "Semigroup law checks" $ do
    it "should check Trivial associativity law" $ do
      runQuickCheckWithHSpec (associativityLaw :: Trivial -> Trivial -> Trivial -> Bool)

    it "should check Identity associativity law" $ do
      runQuickCheckWithHSpec (associativityLaw :: Identity String -> Identity String -> Identity String -> Bool)

    it "should check Two associativity law" $ do
      runQuickCheckWithHSpec
        ( associativityLaw ::
            Two String (Sum Int) ->
            Two String (Sum Int) ->
            Two String (Sum Int) ->
            Bool
        )

    it "should check Three associativity law" $ do
      runQuickCheckWithHSpec
        ( associativityLaw ::
            Three String (Sum Int) String ->
            Three String (Sum Int) String ->
            Three String (Sum Int) String ->
            Bool
        )

    it "should check Four associativity law" $ do
      runQuickCheckWithHSpec
        ( associativityLaw ::
            Four String (Sum Int) String (Product Int) ->
            Four String (Sum Int) String (Product Int) ->
            Four String (Sum Int) String (Product Int) ->
            Bool
        )

    it "should combine BoolConj (both true)" $ do
      BoolConj True <> BoolConj True `shouldBe` BoolConj True
    it "should combine BoolConj (true and false)" $ do
      BoolConj True <> BoolConj False `shouldBe` BoolConj False

    it "should check BoolConj associativity law" $ do
      runQuickCheckWithHSpec (associativityLaw :: BoolConj -> BoolConj -> BoolConj -> Bool)

    it "should combine BoolDisj (both true)" $ do
      BoolDisj True <> BoolDisj True `shouldBe` BoolDisj True
    it "should combine BoolDisj (false and true)" $ do
      BoolDisj True <> BoolDisj False `shouldBe` BoolDisj True

    it "should check BoolDisj associativity law" $ do
      runQuickCheckWithHSpec (associativityLaw :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)

    it "should combine Or (both Fst)" $ do
      Fst 1 <> Fst 2 `shouldBe` (Fst 2 :: (Or Int Int))
    it "should combine Or (Snd and Fst)" $ do
      Snd 1 <> Fst 2 `shouldBe` (Snd 1 :: (Or Int Int))
    it "should combine Or (both Snd)" $ do
      Snd 1 <> Snd 2 `shouldBe` (Snd 1 :: (Or Int Int))

    it "should check Or associativity law" $ do
      runQuickCheckWithHSpec (associativityLaw :: Or Int String -> Or Int String -> Or Int String -> Bool)

    it "should combine two functions wrapped by Combine" $
      let f = Combine $ \n -> Sum (n + 5)
          g = Combine $ \n -> Sum (n - 4)
       in do
            unCombine (f <> g) 0 `shouldBe` Sum 1
            unCombine (f <> g) 1 `shouldBe` Sum 3
            unCombine (f <> f) 1 `shouldBe` Sum 12
            unCombine (g <> f) 1 `shouldBe` Sum 3

    it "should check Combine associativity law" $ do
      runQuickCheckWithHSpec (associativityLaw :: Combine Bool String -> Combine Bool String -> Combine Bool String -> Bool)

    it "should combine two functions wrapped by Comp" $
      let f = Comp $ \n -> n + 5 :: Sum Int
          g = Comp $ \n -> n - 1 :: Sum Int
       in do
            unComp (f <> g) 0 `shouldBe` Sum 4
            unComp (f <> g) 10 `shouldBe` Sum 24
            unComp (g <> f) 13 `shouldBe` Sum 30

    it "should combine Validation success and failure (left side)" $
      Success' 1 <> Failure' "fail" `shouldBe` (Success' 1 :: (Validation String Int))
    it "should combine Validation success and failure (right side)" $
      Failure' "fail" <> Success' 1 `shouldBe` (Success' 1 :: (Validation String Int))
    it "should combine Validation successes" $
      Success' 1 <> Success' 2 `shouldBe` (Success' 1 :: (Validation String Int))
    it "should combine Validation failures" $
      Failure' "blah" <> Failure' "woot" `shouldBe` (Failure' "blahwoot" :: (Validation String Int))
