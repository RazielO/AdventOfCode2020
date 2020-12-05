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

passportAsTuples :: [String] -> [(String, String)]
passportAsTuples = map (\x -> (take 3 x, drop 4 x))

validNumber :: Int -> Int -> Int -> Bool
validNumber n lower upper = lower <= n && n <= upper

validHairColor :: String -> Bool
validHairColor h =
  all (\c -> '0' <= c && c <= '9' || 'a' <= c && c <= 'f') (tail h)
    && head h == '#'

validEyeColor :: String -> Bool
validEyeColor ecl = ecl `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validPassport2 :: [(String, String)] -> [Bool]
validPassport2 [] = []
validPassport2 ((k, v) : xn)
  | k == "byr" = validNumber (read v :: Int) 1920 2002 : validPassport2 xn
  | k == "iyr" = validNumber (read v :: Int) 2010 2020 : validPassport2 xn
  | k == "eyr" = validNumber (read v :: Int) 2020 2030 : validPassport2 xn
  | k == "hcl" = validHairColor v : validPassport2 xn
  | k == "ecl" = validEyeColor v : validPassport2 xn
  | k == "pid" = (length v == 9) : validPassport2 xn
  | k == "hgt" && drop (length v - 2) v == "in" =
    validNumber (read (take (length v - 2) v) :: Int) 59 76 : validPassport2 xn
  | k == "hgt" && drop (length v - 2) v == "cm" =
    validNumber (read (take (length v - 2) v) :: Int) 150 193 : validPassport2 xn
  | otherwise = False : validPassport2 xn

solve2 :: [String] -> [Bool]
solve2 lines = map (\x -> length x == 7 && and x) result
  where
    tuples = map (passportAsTuples . removeOptional . splitPassport) lines
    result = map validPassport2 tuples

main :: IO ()
main = do
  input <- readFile "input.txt"
  let lines = splitOn "\n\n" input
  putStrLn "Part One Solution:"
  print $ length $ filter (== True) $ map validPassport lines
  putStrLn "Part Two Solution:"
  print $ length $ filter (== True) $ solve2 lines