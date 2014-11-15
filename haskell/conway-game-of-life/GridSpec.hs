module GridSpec where

import Test.Hspec
import Grid

main :: IO()
main = hspec $ do
    describe "Given a two-dimensional grid, singly-linked in each direction" $do
        context "which is empty, when I push a list onto its side" $do
            it "should have one column" $do
                let oneColumn = pushAside empty [1, 2, 3]
                listRows oneColumn `shouldBe` [[1], [2], [3]]
