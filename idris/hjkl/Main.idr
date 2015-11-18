module Main

import Data.Vect

import hjkl

main : IO()
main = do
  args <- getArgs
  case args of
       [] => putStrLn "Empty! How did you do that?!"
       (arg :: []) => putStrLn "Yes? What do you want?"
       (_ :: arg :: _) => putStr $ unlines $ writeToList $ readFromList $ lines (!(readFile arg))
