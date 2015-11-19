module ZipperExamples where

import Test.Hspec
import Test.QuickCheck

data ListZipper a =
  Something
  | EmptyZipper
  | Zipper [a] a [a]
  deriving (Show, Eq)

instance Functor ListZipper where
  fmap _ EmptyZipper = EmptyZipper
  fmap f (Zipper left focus right) = Zipper (map f left) (f focus) (map f right)

data Movement = LeftBy Int | RightBy Int

move :: ListZipper a -> Movement -> ListZipper a
move EmptyZipper _ = EmptyZipper
move zipper (LeftBy 0) = zipper
move zipper (RightBy 0) = zipper
move zipper@(Zipper [] x xs) (LeftBy _) = zipper
move zipper@(Zipper xs x []) (RightBy _) = zipper
move zipper@(Zipper (l:ls) x rs) (LeftBy n) = move (Zipper ls l $ x:rs) $ LeftBy $ n - 1
move zipper@(Zipper ls x (r:rs)) (RightBy n) = move (Zipper (x:ls) r rs) $ RightBy $ n - 1

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
  describe "The zipper's functor instance" $ do
    it "abides the identity application law" $ do
      property $ forAll arbitrary_non_empty_zipper $ \zipper ->
        fmap id zipper == zipper
    it "abides the identity application law for empty" $ do
      let exampleEmptyZipper = EmptyZipper :: ListZipper Int
      fmap id exampleEmptyZipper `shouldBe` exampleEmptyZipper

    it "abides composition for some examples" $ do
      property $ \n m ->
        forAll arbitrary_non_empty_zipper $ \zipper ->
          fmap ((+ n) . (+ m)) zipper == (fmap (+ n) . fmap (+ m)) zipper &&
          fmap (show . (+ m)) zipper == (fmap show . fmap (+ m)) zipper

    it "abides composition for some examples on empty" $ do
      let z = EmptyZipper :: ListZipper Int
      property $ \n m ->
        fmap ((+ n) . (+ m)) z == (fmap (+ n) . fmap (+ m)) z &&
        fmap (show . (+ m)) z == (fmap show . fmap (+ m)) z

--  fmap (p . q) = (fmap p) . (fmap q)
  describe "ListZipper" $ do
    describe "move" $ do
      describe "to the left" $ do
        describe "by 0" $ do
          it "does not modify the zipper" $ do
           property $ forAll arbitrary_non_empty_zipper $ \zipper -> move zipper (LeftBy 0) == zipper

        describe "by 1, in the middle of a zipper" $ do
          it "scoots the values over correctly" $ do
           property $ \lefts initialFocus rights ->
             not (null lefts) ==>
               let
                 (newFocus:lefts') = lefts
               in
                 move (Zipper lefts (initialFocus :: Int) rights) (LeftBy 1) == Zipper lefts' newFocus (initialFocus:rights)

        describe "by any amount on the empty zipper" $ do
          it "does not modify the zipper" $ do
           property $ \n ->
             n >= 0 ==> move (EmptyZipper :: ListZipper Int) (LeftBy n) == EmptyZipper

        describe "by any amount, on a zipper focusing on the first element" $ do
          it "does not modify the zipper" $ do
           property $ \x xs n ->
             n > 0 ==> move (Zipper [] (x :: Int) xs) (LeftBy n) == Zipper [] x xs

        describe "by n" $ do
          it "is the same as moving to the left by 1, n times" $ do
            property $ forAll (elements [0..1000]) $ \n ->
              forAll arbitrary_non_empty_zipper $ \zipper ->
                move zipper (LeftBy n) == (iterate (flip move $ LeftBy 1) zipper) !! n

      describe "to the right" $ do
        describe "by 0" $ do
          it "does not modify the zipper" $ do
           property $ forAll arbitrary_non_empty_zipper $ \zipper -> move zipper (RightBy 0) == zipper

        describe "by 1, in the middle of a zipper" $ do
          it "scoots the values over correctly" $ do
           property $ \lefts initialFocus rights ->
             not (null rights) ==>
               let
                 (newFocus:rights') = rights
               in
                 move (Zipper lefts (initialFocus :: Int) rights) (RightBy 1) == Zipper (initialFocus:lefts) newFocus rights'

        describe "by any amount on the empty zipper" $ do
          it "does not modify the zipper" $ do
           property $ \n ->
             n >= 0 ==> move (EmptyZipper :: ListZipper Int) (RightBy n) == EmptyZipper

        describe "by any amount, on a zipper focusing on the final element" $ do
          it "does not modify the zipper" $ do
           property $ \x xs n ->
             n > 0 ==> move (Zipper xs (x :: Int) []) (RightBy n) == Zipper xs x []

        describe "by n" $ do
          it "is the same as moving to the right by 1, n times" $ do
            property $ forAll (elements [0..1000]) $ \n ->
              forAll arbitrary_non_empty_zipper $ \zipper ->
                move zipper (RightBy n) == (iterate (flip move $ RightBy 1) zipper) !! n

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
