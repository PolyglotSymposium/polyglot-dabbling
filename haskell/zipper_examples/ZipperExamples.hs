module ZipperExamples where

import Test.Hspec
import Test.QuickCheck

data ListZipper a =
  Something
  | EmptyZipper
  | Zipper [a] a [a]
  deriving (Show, Eq)

fromList :: [a] -> ListZipper a
fromList [] = EmptyZipper
fromList [v] = Zipper [] v []

main = hspec $ do
  describe "ListZipper" $ do
    describe "fromList" $ do
      describe "given an empty list" $ do
        it "returns the EmptyZipper" $ do
          fromList [] `shouldBe` (EmptyZipper :: ListZipper Int)

      describe "given a singleton list" $ do
        it "returns a proper zipper with empty sides" $ do
          fromList [42] `shouldBe` Zipper [] 42 []
