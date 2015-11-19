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
fromList (x:xs) = Zipper [] x xs

insertFront :: a -> ListZipper a -> ListZipper a
insertFront value EmptyZipper = Zipper [] value []
insertFront value (Zipper start focus rest) = Zipper (start ++ [value]) focus rest

main = hspec $ do
  describe "ListZipper" $ do
    describe "insertFront" $ do
      describe "into empty" $ do
        it "returns a singleton zipper" $ do
          insertFront 42 EmptyZipper `shouldBe` Zipper [] 42 []

      describe "into the singleton" $ do
        it "puts it to the left" $ do
          insertFront 42 (Zipper [] 99 []) `shouldBe` Zipper [42] 99 []

      describe "into a zipper focusing on the first element" $ do
        it "puts it to the left, ignoring the rest" $ do
          insertFront 42 (Zipper [] 99 [1, 2, 3]) `shouldBe` Zipper [42] 99 [1, 2, 3]

      describe "into a zipper focusing on a middle element" $ do
        it "puts it to the leftmost position" $ do
          insertFront 0 (Zipper [3, 2, 1] 4 [5, 6, 7]) `shouldBe` Zipper [3, 2, 1, 0] 4 [5, 6, 7]

    describe "fromList" $ do
      describe "given an empty list" $ do
        it "returns the EmptyZipper" $ do
          fromList [] `shouldBe` (EmptyZipper :: ListZipper Int)

      describe "given a singleton list" $ do
        it "returns a proper zipper with empty sides" $ do
          fromList [42] `shouldBe` Zipper [] 42 []

      describe "given a list with multiple elements" $ do
        it "returns a zipper with the tail on the right" $ do
          fromList [1, 2, 3] `shouldBe` Zipper [] 1 [2, 3]
