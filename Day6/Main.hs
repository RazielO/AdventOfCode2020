module Main where

import Data.List (nub, intersect)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let groups = map (splitOn "\n") (splitOn "\n\n" input)
  putStrLn "Part One Solution:"
  print $ sum $ map (length . nub . concat) groups
  putStrLn "Part Two Solution:"
  print $ sum $ map (\a -> length $ foldr intersect (head a) a) groups
