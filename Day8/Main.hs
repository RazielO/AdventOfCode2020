module Main where

import Data.List (reverse)
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import Text.Printf (printf)

data Instruction = Instruction
  { operation :: String,
    argument :: Int
  }
  deriving (Show)

parse :: [String] -> [Instruction]
parse [] = []
parse (x : xn) = parse xn ++ [Instruction op arg]
  where
    parts = splitOn " " x
    op = head parts
    arg =
      if head (last parts) == '+'
        then read (drop 1 (last parts)) :: Int
        else read (last parts) :: Int

solveOne :: [Instruction] -> [Int] -> Int -> Int -> Int
solveOne instructions executed acc ip
  | ip `elem` executed = acc
  | otherwise = solveOne instructions exec accum pointer
  where
    inst = instructions !! ip
    op = operation inst
    exec = executed ++ [ip]
    accum =
      if op == "acc"
        then acc + argument inst
        else acc
    pointer =
      if op == "jmp"
        then ip + argument inst
        else ip + 1

main :: IO ()
main = do
  input <- readFile "input.txt"
  let instructions = reverse $ parse $ splitOn "\n" input
  putStrLn "Part One Solution:"
  print $ solveOne instructions [] 0 0