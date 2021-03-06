module LifeAndDeathSpecs where

import Test.Hspec
import LifeAndDeath

main :: IO ()
main = hspec $ do
    describe "A cell which was alive in the previous generation" $do
        it "should be dead now if it only had one living neighbor" $do
            nextStateOf LivingCell 1 `shouldBe` DeadCell
        it "should still be living if it had two living neighbors" $do
            nextStateOf LivingCell 2 `shouldBe` LivingCell
        it "should still be living if it had three living neighbors" $do
            nextStateOf LivingCell 3 `shouldBe` LivingCell
        it "should be dead now if it had four living neighborss" $do
            nextStateOf LivingCell 4 `shouldBe` DeadCell
    describe "A cell which was dead in the previous generation" $do
        it "should still be dead if it only had two living neighbors" $do
            nextStateOf DeadCell 2 `shouldBe` DeadCell
        it "should come to life if it had three living neighbors" $do
            nextStateOf DeadCell 3 `shouldBe` LivingCell
        it "should still be dead if it had four living neighbors" $do
            nextStateOf DeadCell 4 `shouldBe` DeadCell
