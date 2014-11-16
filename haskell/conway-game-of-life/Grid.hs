module Grid where

data LinkedList2D a = Terminator
    | Node { valueOf :: a, right :: LinkedList2D a, below :: LinkedList2D a }
    deriving (Eq, Show)

empty = Terminator

topAsList Terminator = []
topAsList node = valueOf node : topAsList (right node)

rowFromList [] = Terminator
rowFromList row = Node {
    valueOf = head row,
    right = rowFromList (tail row),
    below = empty
}

columnFromList [] = Terminator
columnFromList column = Node {
    valueOf = head column,
    right = empty,
    below = columnFromList (tail column)
}

fromRowLists [] = Terminator
fromRowLists rows = pushDown (fromRowLists (tail rows)) (head rows)

pushAside _ newColumn = columnFromList newColumn

pushDown Terminator [] = Terminator
pushDown Terminator row = rowFromList row
pushDown node rowAsList = Node {
    valueOf = head rowAsList,
    right = pushDown (right node) (tail rowAsList),
    below = node
}

innerZipWith :: (a -> a -> b) -> LinkedList2D a -> LinkedList2D a -> LinkedList2D b
innerZipWith _ Terminator _ = empty
innerZipWith _ _ Terminator = empty
innerZipWith _ _ _ = empty

listRows Terminator = []
listRows node = topAsList node : listRows (below node)
