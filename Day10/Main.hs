module Main where

import Data.List (sort)
import Data.List.Split (splitOn)

solveOne :: [Int] -> Int
solveOne jolts = ones * threes
  where
    j = [b - a | (a, b) <- zip jolts (tail jolts)]
    ones = 1 + length (filter (== 1) j)
    threes = 1 + length (filter (== 3) j)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let jolts = map (\x -> read x :: Int) (splitOn "\n" input)
  putStrLn "Part One Solution:"
  print $ solveOne (sort jolts)