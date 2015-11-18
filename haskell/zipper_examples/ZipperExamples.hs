module ZipperExamples where

import Test.Hspec
import Test.QuickCheck

data ListZipper a =
  Something
  | EmptyZipper
  deriving (Show, Eq)

fromList :: [a] -> ListZipper a
fromList = const EmptyZipper

main = hspec $ do
  describe "ListZipper" $ do
    describe "fromList" $ do
      describe "given an empty list" $ do
        it "returns the EmptyZipper" $ do
          fromList [] `shouldBe` EmptyZipper
