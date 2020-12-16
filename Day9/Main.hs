module Main where

import Data.List.Split (splitOn)

findSum :: [Int] -> Int -> (Bool, [Int])
findSum [] _ = (False, [])
findSum (num : ns) target
  | not (null search) = (True, [num, head search])
  | otherwise = findSum ns target
  where
    search = filter (== (target - num)) ns

solveOne :: [Int] -> Int -> Int
solveOne numbers current
  | not (fst try) = numbers !! (current + 25)
  | otherwise = solveOne numbers (current + 1)
  where
    try = findSum (take 25 (drop current numbers)) (numbers !! (current + 25))

main :: IO ()
main = do
  input <- readFile "input.txt"
  let numbers = map (\x -> read x :: Int) (splitOn "\n" input)
  putStrLn "Part One Solution:"
  print $ solveOne numbers 0
