module Main

import Data.Vect

import hjkl

main : IO()
main = do
  x <- readFile "C:/Users/pinsonke/.gitconfig"
  putStr $ unlines $ writeToList $ readFromList $ lines x
