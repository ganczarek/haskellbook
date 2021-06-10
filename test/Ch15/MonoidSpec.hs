module Ch15.MonoidSpec where

import Ch15.ArbitraryTestInstances ()
import Ch15.Laws
import Ch15.Monoid
import Ch15.Semigroup
import Data.Monoid
import Test.Hspec
import Utils (runQuickCheckWithHSpec)

main :: IO ()
main = hspec $ do
  describe "Monoid law checks" $ do
    it "should check Trivial monoid laws" $ do
      runQuickCheckWithHSpec (leftIdentityLaw :: Trivial -> Bool)
      runQuickCheckWithHSpec (rightIdentityLaw :: Trivial -> Bool)

    it "should check Identity monoid laws" $ do
      runQuickCheckWithHSpec (leftIdentityLaw :: Identity String -> Bool)
      runQuickCheckWithHSpec (rightIdentityLaw :: Identity String -> Bool)

    it "should check Two monoid laws" $ do
      runQuickCheckWithHSpec (leftIdentityLaw :: Two String (Sum Int) -> Bool)
      runQuickCheckWithHSpec (rightIdentityLaw :: Two String (Sum Int) -> Bool)

    it "should check BoolConj monoid laws" $ do
      runQuickCheckWithHSpec (leftIdentityLaw :: BoolConj -> Bool)
      runQuickCheckWithHSpec (rightIdentityLaw :: BoolConj -> Bool)

    it "should check BoolDisj monoid laws" $ do
      runQuickCheckWithHSpec (leftIdentityLaw :: BoolDisj -> Bool)
      runQuickCheckWithHSpec (rightIdentityLaw :: BoolDisj -> Bool)

    it "should check Combine left associativity law" $ do
      let f = Combine $ \n -> Sum (n + 3)
       in do
            unCombine (mempty <> f) 0 `shouldBe` unCombine f 0
            unCombine (mempty <> f) 5 `shouldBe` unCombine f 5
    it "should check Combine right associativity law" $ do
      let f = Combine $ \n -> Sum (n + 3)
       in do
            unCombine (f <> mempty) 0 `shouldBe` unCombine f 0
            unCombine (f <> mempty) 5 `shouldBe` unCombine f 5

    it "should check Comp left associativity law" $ do
      let f = Comp $ \n -> n + 5 :: Sum Int
       in do
            unComp (mempty <> f) 0 `shouldBe` unComp f 0
            unComp (mempty <> f) 5 `shouldBe` unComp f 5
    it "should check Combine right associativity law" $ do
      let f = Comp $ \n -> n + 5 :: Sum Int
       in do
            unComp (f <> mempty) 0 `shouldBe` unComp f 0
            unComp (f <> mempty) 5 `shouldBe` unComp f 5

    it "should check Mem associativity laws" $ do
      let f = Mem $ \s -> ("hi", s + 1)
       in do
            runMem (f <> mempty) 0 `shouldBe` ("hi", 1)
            runMem (mempty <> f) 0 `shouldBe` ("hi", 1)
            runMem mempty 0 `shouldBe` ("", 0)

    it "should combine Mem" $ do
      let f = Mem $ \s -> ("Hi", s + 1)
          g = Mem $ \s -> ("Hello", s * 2)
       in do
            runMem (f <> g) 4 `shouldBe` ("HiHello", 10)
            runMem (g <> f) 4 `shouldBe` ("HelloHi", 9)
