module LifeAndDeathSpecs where

import Test.Hspec
import LifeAndDeath

main :: IO ()
main = hspec $ do
    describe "A cell which was alive in the previous generation" $do
        it "should be dead now if it only had one living neighbor" $do
            nextStateOf LivingCell 1 `shouldBe` DeadCell
