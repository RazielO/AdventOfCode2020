module Main where

import Data.List (reverse)
import Data.List.Split (splitOn)
import Data.Char (intToDigit)
import Data.Maybe
import qualified Data.IntMap as IM

data Instruction
  = Mask {mask :: String}
  | Memory {position :: Int, value :: Integer}
  deriving (Show)

data Computer = Computer
  { mask' :: Instruction,
    memory' :: IM.IntMap Integer
  }
  deriving (Show)

binaryToInteger :: String -> Integer
binaryToInteger "" = 0
binaryToInteger str@(x : xs) = binaryToInteger xs + value
  where
    value = (2 ^ (length str - 1)) * n
    n = read [x] :: Integer

integerToBinary' :: Integer -> [Integer]
integerToBinary' 0 = []
integerToBinary' n = remainder : integerToBinary' quotient
  where
    (quotient, remainder) = n `divMod` 2

integerToBinary :: Integer -> String
integerToBinary 0 = "0"
integerToBinary n = concatMap show (reverse (integerToBinary' n))

applyMask :: String -> String -> String
applyMask mask value
  | length value < 36 = applyMask mask (replicate (36 - length value) '0' ++ value)
  | otherwise = [if m == 'X' then v else m | (m, v) <- zip mask value]

parseInstructions :: [String] -> [Instruction]
parseInstructions [] = []
parseInstructions (line : rest)
  | take 4 line == "mask" = Mask value' : parseInstructions rest
  | otherwise =
    Memory position' (read value' :: Integer) :
    parseInstructions rest
  where
    value' = tail $ last $ splitOn "=" line
    position' = read (init $ drop 4 $ init $ head $ splitOn "=" line) :: Int

updateMemory :: Computer -> Instruction -> Computer
updateMemory computer@(Computer mask' slots) inst@(Memory p v) = computer'
  where
    idx = p `IM.lookup` slots
    inst' = binaryToInteger $ applyMask (mask mask') (integerToBinary v)
    slots' = IM.insert p inst' slots
    computer' = Computer mask' slots'

doInstruction :: Computer -> Instruction -> Computer
doInstruction computer mask'@(Mask _) = Computer mask' (memory' computer)
doInstruction computer inst = updateMemory computer inst

solveOne :: Computer -> [Instruction] -> Integer
solveOne computer [] = IM.foldl (+) 0 (memory' computer)
solveOne computer (x : xs) = solveOne computer' xs
  where
    computer' = doInstruction computer x

truthTable :: [Int] -> [[Int]]
truthTable [] = [[]]
truthTable (x : xs) = map (1 : ) ts ++ map (0 : ) ts
  where 
    ts = truthTable xs

replaceFloating :: String -> [Int] -> String
replaceFloating [] _ = ""
replaceFloating (x : xs) v
  | x /= 'X' = x : replaceFloating xs v
  | otherwise = intToDigit (head v) : replaceFloating xs (tail v)

applyMaskV2 :: String -> String -> [Integer]
applyMaskV2 mask val
  | length val < 36 = applyMaskV2 mask (replicate (36 - length val) '0' ++ val)
  | otherwise = map (binaryToInteger . replaceFloating m) vals
  where
    m = [if m == '0' then v else m | (m, v) <- zip mask val]
    nums = length $ filter (== 'X') m
    vals = truthTable [0 .. nums - 1]

updateMemoryV2' :: Computer -> Instruction -> Computer
updateMemoryV2' computer@(Computer mask' slots) inst@(Memory p v) = computer'
  where
    idx = p `IM.lookup` slots
    slots' = IM.insert p v slots
    computer' = Computer mask' slots'

updateMemoryV2 :: Computer -> [Instruction] -> Computer
updateMemoryV2 computer [] = computer
updateMemoryV2 computer (x : xs) = updateMemoryV2 computer' xs
  where
    computer' = updateMemoryV2' computer x

doInstructionV2 :: Computer -> Instruction -> Computer
doInstructionV2 computer mask'@(Mask _) = Computer mask' (memory' computer)
doInstructionV2 computer@(Computer m' _) inst@(Memory p v) = updateMemoryV2 computer inst'
  where
    inst' = map (\x -> Memory (fromIntegral x) v) (applyMaskV2 (mask m') (integerToBinary (fromIntegral p)))

solveTwo :: Computer -> [Instruction] -> Integer
solveTwo computer [] = IM.foldl (+) 0 (memory' computer)
solveTwo computer (x : xs) = solveTwo computer' xs
  where
    computer' = doInstructionV2 computer x

main :: IO ()
main = do
  input <- readFile "input.txt"
  let instructions = parseInstructions (splitOn "\n" input)
  putStrLn "Part One Solution:"
  print $ solveOne (Computer (Mask "") IM.empty) instructions
  putStrLn "Part Two Solution:"
  print $ solveTwo (Computer (Mask "") IM.empty) instructions