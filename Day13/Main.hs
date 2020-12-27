module Main where

import Data.List.Split (splitOn)

nextBus :: Int -> Int -> (Int, Int)
nextBus time id = (id, ((time `div` id) + 1) * id - time)

solveOne :: Int -> [Int] -> Int
solveOne time ids = busId * busTime
  where
    buses = map (nextBus time) ids
    values = filter (\x -> snd x == minimum (map snd buses)) buses
    busId = fst (head values)
    busTime = snd (head values)

getData :: [Char] -> (Int, [Int])
getData input = (time, buses)
  where
    lines = splitOn "\n" input
    time = read (head lines) :: Int
    buses = map (\x -> read x :: Int) (filter (/= "x") (splitOn "," (last lines)))

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (minTime, buses) = getData input
  putStrLn "Part One Solution:"
  print $ solveOne minTime buses