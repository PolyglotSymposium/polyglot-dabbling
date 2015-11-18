module ZipperExamples where

import Test.Hspec
import Test.QuickCheck

data ListZipper a = Something
  deriving (Show, Eq)

fromList :: [a] -> ListZipper a
fromList = const Something

main = hspec $ do
  describe "ListZipper" $ do
    it "exists" $ do
      fromList [] `shouldBe` fromList []
