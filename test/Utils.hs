module Utils where

import Test.Hspec
import Test.QuickCheck

runQuickCheckWithHSpec :: Testable prop => prop -> IO ()
runQuickCheckWithHSpec testProp = do
  result <- quickCheckResult testProp
  show result `shouldStartWith` "Success"
