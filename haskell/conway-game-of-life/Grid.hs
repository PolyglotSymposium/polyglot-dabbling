module Grid where

data LinkedList2D a = Terminator
    | Node { valueOf :: a, right :: LinkedList2D a, below :: LinkedList2D a }
    deriving (Eq, Show)

empty = Terminator

topAsList Terminator = []
topAsList node = valueOf node : topAsList (right node)

leftAsList _ = [1, 2, 3]

rowFromList [] = Terminator
rowFromList row = Node { valueOf = head row, right = rowFromList (tail row), below = empty }

--columnFromList [] = Terminator
columnFromList column = Node { valueOf = head column, right = empty, below = columnFromList (tail column) }

fromRowLists _ = [[1, 2], [2, 4]]

pushAside _ newColumn = columnFromList newColumn

pushDown _ _ = [1, 2, 3]

innerZipWith _ _ _ = empty

listRows grid = grid
