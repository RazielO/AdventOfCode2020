import System.IO


{-
  Since the input is a file with numbers separated by new lines,
  the first thing is to change new lines to commas, add brackets
  and parse it to a list
-}
parseInput :: String -> [Int]
parseInput input = read ("[" ++ map (\x -> if x == '\n' then ',' else x) input ++ "]") :: [Int]

{-
  For each number on the list, take away the target and append it
  to a list, if the current number is on that list, it means the
  target is found, so, the answer is x * (target - x)
-}
solve' :: [Int] -> [Int] -> Int -> Int
solve' seen (x:xn) target
  | elem x seen = (target - x) * x
  | otherwise   = solve' (seen ++ [target - x]) xn target


{-
  Auxiliar function to call the solve' function
-}
solve :: [Int] -> Int -> Int
solve numbers target = solve' [] numbers target


main :: IO ()
main = do
  input <- readFile "input1.txt"
  print $ solve (parseInput input) 2020
