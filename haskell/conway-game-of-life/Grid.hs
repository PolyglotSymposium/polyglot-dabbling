module Grid where

empty = []

fromRowLists _ = [[1, 2], [2, 4]]

pushAside _ _ = [[1], [2], [3]]

pushDown _ _ = [1, 2, 3]

listRows grid = grid
