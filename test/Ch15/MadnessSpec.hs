module Ch15.MadnessSpec where

import Ch15.Madness
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "Mad Libs" $ do
    it "returns the same result for both implementations" $ do
      madlibbin' "Hey" "suddenly" "seat" "beautiful" `shouldBe` madlibbinBetter' "Hey" "suddenly" "seat" "beautiful"

    it "returns correctly formatted sentence" $ do
      madlibbinBetter' "Hey" "suddenly" "seat" "beautiful" `shouldBe` "Hey! he said suddenly as he jumped into his car seat and drove off with his beautiful wife."
