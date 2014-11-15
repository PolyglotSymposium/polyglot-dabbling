module GridSpec where

import Test.Hspec
import Grid

main :: IO()
main = hspec $ do
    describe "Given a two-dimensional grid, singly-linked in each direction" $do
        context "which is empty, when I push a list onto its side" $do
            it "should have one column" $do
                let grid = pushAside empty [1, 2, 3]
                listRows grid `shouldBe` [[1], [2], [3]]
        context "which is empty, when I push a list onto its top" $do
            it "should have one column" $do
                let grid = pushDown empty [1, 2, 3]
                listRows grid `shouldBe` [1, 2, 3]
        context "which has been initialized from a list of lists" $do
            it "should be able to be converted back to the list of lists" $do
                let grid = fromRowLists [[1, 2], [2, 4]]
                listRows grid `shouldBe` [[1, 2], [2, 4]]
        context "which is empty, when I inner-zip a grid that is not empty onto it" $do
            it "should produce an empty grid" $do
                innerZip empty (fromRowLists [[2]]) `shouldBe` empty
