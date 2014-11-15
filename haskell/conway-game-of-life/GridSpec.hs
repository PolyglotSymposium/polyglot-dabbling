module GridSpec where

import Test.Hspec
import Grid

main :: IO()
main = hspec $ do
    describe "Given a two-dimensional grid, singly-linked in each direction" $do
        context "which is empty, when I push a list of three elements onto its side" $do
            let grid = pushAside empty [1, 2, 3]
            it "should have one column" $do
                leftAsList grid `shouldBe` [1, 2, 3]
            it "should have a top row equal to the first element of the list" $do
                topAsList grid `shouldBe` [1]
            --it "should have a middle row equal to the second element of the list" $do
            --    topAsList (topAsList grid) `shouldBe` [2]
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
                innerZipWith (+) empty (fromRowLists [[2]]) `shouldBe` empty
        context "which is not empty, when I inner-zip a grid that is the same size onto it" $do
            it "should produce a combination of the two grids" $do
                pending
                --let grid1 = fromRowLists [[True, False], [False, False]]
                --let grid2 = fromRowLists [[False, False], [False, True]]
                --innerZipWith (||) grid1 grid2 `shouldBe` fromRowLists [[True, False], [False, True]]
