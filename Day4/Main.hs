module Main where

import Data.List.Split (splitOn)

mandatoryFields :: [String]
mandatoryFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

optionalFields :: [String]
optionalFields = ["cid"]

splitPassport :: String -> [String]
splitPassport line = concatMap (splitOn " ") (splitOn "\n" line)

validFields :: [String] -> [Bool]
validFields = map (\x -> take 3 x `elem` mandatoryFields)

removeOptional :: [String] -> [String]
removeOptional = filter (\x -> take 3 x `notElem` optionalFields)

validPassport :: String -> Bool
validPassport line = and list && length list == length mandatoryFields
  where
    list = validFields $ removeOptional $ splitPassport line

main :: IO ()
main = do
  input <- readFile "input.txt"
  let lines = splitOn "\n\n" input
  putStrLn "Part One Solution:"
  print $ length $ filter (== True) $ map validPassport lines