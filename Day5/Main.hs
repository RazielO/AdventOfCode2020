module Main where

import Data.List.Split (splitOn)

getRow :: String -> Int -> Int -> Int
getRow "" low high = high
getRow (x : xs) low high
  | x == 'F' = getRow xs low ((low + high) `div` 2)
  | x == 'B' = getRow xs ((low + high) `div` 2) high

getColumn :: String -> Int -> Int -> Int
getColumn "" low high = high
getColumn (x : xs) low high
  | x == 'L' = getColumn xs low ((low + high) `div` 2)
  | x == 'R' = getColumn xs ((low + high) `div` 2) high

getSeatId :: String -> Int
getSeatId pass = getRow (take 7 pass) 0 127 * 8 + getColumn (drop 7 pass) 0 7

main :: IO ()
main = print "Hello"
main = do
  input <- readFile "input.txt"
  let lines = splitOn "\n" input
  putStrLn "Part One Solution:"
  print $ maximum $ map getSeatId lines