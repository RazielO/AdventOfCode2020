module Main where

import Data.List.Split (splitOn)
import Debug.Trace (trace)

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

solveTwo' :: [Int] -> Int -> Int -> Int -> (Bool, [Int])
solveTwo' numbers start size target
  | sum nums == target = (True, nums)
  | sum nums > target = (False, [])
  | otherwise = solveTwo' numbers start (size + 1) target
  where
    nums = take size (drop start numbers)

solveTwo :: [Int] -> Int -> Int -> Int
solveTwo [] _ _ = -1
solveTwo numbers target start
  | fst try = minimum (snd try) + maximum (snd try)
  | otherwise = solveTwo numbers target (start + 1)
    where
      try = solveTwo' numbers start 0 target

main :: IO ()
main = do
  input <- readFile "input.txt"
  let numbers = map (\x -> read x :: Int) (splitOn "\n" input)
  let first = solveOne numbers 0
  putStrLn "Part One Solution:"
  print first
  putStrLn "Part Two Solution:"
  print $ solveTwo (filter (/= first) numbers) first 0
