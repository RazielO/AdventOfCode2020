module Main where

import qualified Data.IntMap.Strict as M
import Data.List (sort, tails)
import Data.List.Split (splitOn)

solveOne :: [Int] -> Int
solveOne jolts = ones * threes
  where
    j = [b - a | (a, b) <- zip jolts (tail jolts)]
    ones = 1 + length (filter (== 1) j)
    threes = 1 + length (filter (== 3) j)

type Cache = M.IntMap Int

solveTwo' :: Cache -> Int -> [Int] -> (Cache, Int)
solveTwo' cache _ [] = (cache, 0)
solveTwo' cache previous (current : adapters)
  | current - previous > 3 = (cache, 0)
  | null adapters = (cache, 1)
  | otherwise =
    case cache M.!? current of
      Just cached -> (cache, cached)
      Nothing ->
        let step (c, r) ns = (r +) <$> solveTwo' c current ns
            (cache', result) = foldl step (cache, 0) $ take 3 $ tails adapters
            newCache = M.insert current result cache'
         in (newCache, result)

solveTwo :: [Int] -> Int
solveTwo jolts = snd $ solveTwo' M.empty 0 jolts

main :: IO ()
main = do
  input <- readFile "input.txt"
  let jolts = map (\x -> read x :: Int) (splitOn "\n" input)
  putStrLn "Part One Solution:"
  print $ solveOne (sort jolts)
  putStrLn "Part Two Solution:"
  print $ solveTwo (sort $ jolts ++ [0, maximum jolts + 3])