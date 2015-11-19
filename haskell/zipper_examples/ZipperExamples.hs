module ZipperExamples where

import Test.Hspec
import Test.QuickCheck

data ListZipper a =
  Something
  | EmptyZipper
  | Zipper [a] a [a]
  deriving (Show, Eq)

data Movement = LeftBy Int | RightBy Int

move :: ListZipper a -> Movement -> ListZipper a
move EmptyZipper _ = EmptyZipper
move zipper (LeftBy 0) = zipper
move zipper (RightBy 0) = zipper
move zipper@(Zipper [] x xs) (LeftBy _) = zipper
move zipper@(Zipper xs x []) (RightBy _) = zipper

fromList :: [a] -> ListZipper a
fromList [] = EmptyZipper
fromList (x:xs) = Zipper [] x xs

insertFront :: a -> ListZipper a -> ListZipper a
insertFront value EmptyZipper = Zipper [] value []
insertFront value (Zipper start focus rest) = Zipper (start ++ [value]) focus rest

arbitrary_non_empty_zipper :: Gen (ListZipper Int)
arbitrary_non_empty_zipper = do
  start <- arbitrary
  rest <- arbitrary
  focus <- arbitrary
  return $ Zipper start focus rest

main :: IO ()
main = hspec $ do
  describe "ListZipper" $ do
    describe "move" $ do
      describe "to the left" $ do
        describe "by 0" $ do
          it "does not modify the zipper" $ do
           property $ forAll arbitrary_non_empty_zipper $ \zipper -> move zipper (LeftBy 0) == zipper

        describe "by any amount on the empty zipper" $ do
          it "does not modify the zipper" $ do
           property $ \n ->
             n >= 0 ==> move (EmptyZipper :: ListZipper Int) (LeftBy n) == EmptyZipper

        describe "by any amount, on a zipper focusing on the first element" $ do
          it "does not modify the zipper" $ do
           property $ \x xs n ->
             n > 0 ==> move (Zipper [] (x :: Int) xs) (LeftBy n) == Zipper [] x xs

      describe "to the right" $ do
        describe "by 0" $ do
          it "does not modify the zipper" $ do
           property $ forAll arbitrary_non_empty_zipper $ \zipper -> move zipper (RightBy 0) == zipper

        describe "by any amount on the empty zipper" $ do
          it "does not modify the zipper" $ do
           property $ \n ->
             n >= 0 ==> move (EmptyZipper :: ListZipper Int) (RightBy n) == EmptyZipper

        describe "by any amount, on a zipper focusing on the final element" $ do
          it "does not modify the zipper" $ do
           property $ \x xs n ->
             n > 0 ==> move (Zipper xs (x :: Int) []) (RightBy n) == Zipper xs x []


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
