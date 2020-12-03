import Data.List (nub)
import System.IO (readFile)

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
  target is found, so, the answer is the list of x and target - x
-}
solve' :: [Int] -> [Int] -> Int -> [Int]
solve' seen [] target = []
solve' seen (x : xn) target
  | x `elem` seen = [target - x, x]
  | otherwise = solve' (seen ++ [target - x]) xn target

{-
  Auxiliar function to call the solve' function
-}
solve :: [Int] -> Int -> Int
solve numbers target = product $ solve' [] numbers target

{-
  Solution for part two.
  Essentially, is the same solution for part one, but since
  it needs three numbers, first take away n from the target
  and call the solution for part one but target it's the
  remainder
-}
solve2 :: [Int] -> Int -> Int
solve2 input target =
  product $
    nub $
      concat $
        filter
          (not . null)
          (solve2' input [(x, target - x) | x <- input])

{-
  Call the solve' function with all the targets and lists
  generated in solve2
-}
solve2' :: [Int] -> [(Int, Int)] -> [[Int]]
solve2' input = map (\x -> solve' [] (filter (/= fst x) input) (snd x))

main :: IO ()
main = do
  input <- readFile "input.txt"
  putStrLn "Solution of part 1:"
  print $ solve (parseInput input) 2020
  putStrLn "Solution of part 2:"
  print $ solve2 (parseInput input) 2020
