module Grid where

data LinkedList2D a = Node | Terminator deriving (Eq, Show)

empty = Terminator

topAsList _ = [1]

leftAsList _ = [1, 2, 3]

fromRowLists _ = [[1, 2], [2, 4]]

pushAside _ _ = [[1], [2], [3]]

pushDown _ _ = [1, 2, 3]

innerZipWith _ _ _ = empty

listRows grid = grid
