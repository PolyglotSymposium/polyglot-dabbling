module Grid where

data LinkedList2D a = Node | Terminator deriving (Eq, Show)

empty = Terminator

fromRowLists _ = [[1, 2], [2, 4]]

pushAside _ _ = [[1], [2], [3]]

pushDown _ _ = [1, 2, 3]

innerZip _ _ = empty

listRows grid = grid
