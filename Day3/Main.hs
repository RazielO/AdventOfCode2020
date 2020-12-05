module Main where

import Data.List.Split (splitOn)

isATree :: String -> Int -> Bool
isATree line n = line !! pos == '#'
  where
    pos = (n * 3) `mod` length line

solve :: [String] -> Int
solve lines =
  length $
    filter (== True) $
      zipWith isATree lines [0 ..]

main :: IO ()
main = do
  input <- readFile "input.txt"
  let lines = splitOn "\n" input

  print "Part One Solution:"
  print $ solve lines