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

execute :: [Instruction] -> [Int] -> Int -> Int -> (Bool, Int)
execute instructions executed acc ip
  | ip `elem` executed = (False, acc)
  | ip >= length instructions = (True, acc)
  | otherwise = execute instructions exec accum pointer
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

solveTwo :: [Instruction] -> [(Int, Instruction)] -> Int
solveTwo instructions ((i, v) : xn)
  | fst execution = snd execution
  | otherwise = solveTwo instructions xn
  where
    changed = if operation v == "nop"
                then Instruction "jmp" (argument v)
                else Instruction "nop" (argument v)
    instr = take i instructions ++ [changed] ++ drop (i + 1) instructions
    execution = execute instr [] 0 0

main :: IO ()
main = do
  input <- readFile "input.txt"
  let instructions = reverse $ parse $ splitOn "\n" input
  putStrLn "Part One Solution:"
  print $ snd $ execute instructions [] 0 0
  putStrLn "Part Two Solution:"
  print $ solveTwo instructions (zip [0..] instructions)