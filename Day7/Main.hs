module Main where

import Data.Char (isNumber)
import Data.List (delete, intersect, nub)
import Data.List.Split (splitOn)

data Bag = Bag
  { color :: String,
    content :: [(Int, String)],
    value :: Int
  }
  deriving (Show, Eq)

removePunctuation :: String -> String
removePunctuation = filter (\x -> x /= '.' && x /= ',')

cleanLine :: [String] -> [String]
cleanLine line =
  filter
    (/= "contain")
    (map (\x -> if x == "bag" then "bags" else x) line)

parseBag :: [String] -> [(Int, String)] -> Bag
parseBag [] content = Bag (snd $ head content) (filter (\y -> snd y /= "no other") (tail content)) (-1)
parseBag line content = parseBag (drop (length part + 1) line) (content ++ [c])
  where
    part = takeWhile (/= "bags") line
    c
      | isNumber $ head $ head part = (read (head part) :: Int, unwords $ tail part)
      | otherwise = (0, unwords part)

search :: [Bag] -> String -> [Bag]
search bags term = filter (any (\y -> snd y == term) . content) bags

solveOne :: [Bag] -> [String] -> [Bag]
solveOne _ [] = []
solveOne bags terms = do
  let b = nub $ concatMap (search bags) terms
  let t = map color b
  solveOne bags t ++ b

calculate :: [Bag] -> String -> Int
calculate bags term = do
  let bag = head $ filter (\x -> color x == term) bags
  if null (content bag)
    then 0
    else if value bag /= (-1)
      then value bag
      else sum (map (\x -> fst x + (fst x * calculate bags (snd x))) (content bag))

main :: IO ()
main = do
  input <- readFile "input.txt"
  let lines = map (splitOn " " . removePunctuation) (splitOn "\n" input)
  let bags = map (\x -> parseBag (cleanLine x) []) lines
  putStrLn "Part One Solution:"
  print $ length $ nub $ solveOne bags ["shiny gold"]
  putStrLn "Part Two Solution:"
  print $ calculate bags "shiny gold"