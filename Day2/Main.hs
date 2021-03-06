module Main where

import Data.List.Split (splitOn)

data Entry = Entry
  { lower :: Int,
    upper :: Int,
    letter :: Char,
    password :: String
  } deriving (Show)

parseLine :: String -> Entry
parseLine line = Entry lower upper letter password
  where
    aux = splitOn " " line
    numbers = splitOn "-" (head aux)
    lower = read (head numbers) :: Int
    upper = read (last numbers) :: Int
    letter = head $ aux !! 1
    password = last aux

count :: Entry -> Int
count entry = length $ filter (\x -> x == letter entry) (password entry)

isValid :: Entry -> Bool
isValid entry = lower entry <= count entry && count entry <= upper entry

isValid2 :: Entry -> Bool
isValid2 entry = first /= second
  where
    first = password entry !! (lower entry - 1) == letter entry
    second = password entry !! (upper entry - 1) == letter entry

main :: IO ()
main = do
  input <- readFile "input.txt"
  let entries = map parseLine $ splitOn "\n" input
  putStrLn "Result of Part One:"
  print $ length $ filter isValid entries
  putStrLn "Result of Part Two:"
  print $ length $ filter isValid2 entries