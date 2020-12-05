module Main where

import Data.List.Split (splitOn)

isATree :: String -> Int -> Int -> Bool
isATree line n right = line !! pos == '#'
  where
    pos = (n * right) `mod` length line

solve :: [String] -> Int -> Int -> Int
solve lines right down =
  length $
    filter (== True) $
      map (\x -> uncurry isATree x right) $
        filter (\x -> snd x `mod` down == 0) (zip lines [0 ..])

main :: IO ()
main = do
  input <- readFile "input.txt"
  let lines = splitOn "\n" input

  putStrLn "Part One Solution:"
  print $ solve lines 3 1

  putStrLn "Part Two Solution:"
  let slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]
  print $ product $ map (uncurry (solve lines)) slopes