module Main where

import Data.List (reverse)
import Data.List.Split (splitOn)
import Data.Maybe
import qualified Data.Vector as Vector

data Instruction
  = Mask {mask :: String}
  | Memory {position :: Int, value :: Integer}
  deriving (Show)

data Computer = Computer
  { currentMask :: Instruction,
    memory :: Vector.Vector Instruction
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
  | start == "mask" = Mask value' : parseInstructions rest
  | otherwise =
    Memory position' (read value' :: Integer) :
    parseInstructions rest
  where
    start = take 4 line
    value' = tail $ last $ splitOn "=" line
    position' = read (init $ drop 4 $ init $ head $ splitOn "=" line) :: Int

updateMask :: Computer -> Instruction -> Computer
updateMask c@(Computer mask' memory') newMask = Computer newMask memory'

findMemoryIndex :: Vector.Vector Instruction -> Int -> Maybe Int
findMemoryIndex inst x = idx
  where
    idx
      | Vector.length values == 0 = Nothing
      | otherwise = Just (Vector.head values)
    values = Vector.filter (/= -1) vals
    vals = Vector.imap (\i ins -> if position ins == x then i else -1) inst

updateMemory :: Computer -> Instruction -> Computer
updateMemory computer@(Computer mask' slots) inst@(Memory p v) = computer'
  where
    prevIdx = findMemoryIndex slots (position inst)
    inst' = Memory {position = p, value = binaryToInteger (applyMask (mask mask') (integerToBinary v))}
    slots'
      | isJust prevIdx = Vector.update slots (Vector.fromList [(fromJust prevIdx, inst')])
      | otherwise = Vector.snoc slots inst'
    computer' = Computer {currentMask = mask', memory = slots'}

doInstruction :: Computer -> Instruction -> Computer
doInstruction computer mask'@(Mask val) = updateMask computer mask'
doInstruction computer inst = updateMemory computer inst

solveOne :: Computer -> [Instruction] -> Integer
solveOne computer [] = sum (Vector.map value (memory computer))
solveOne computer (x : xs) = solveOne computer' xs
  where
    computer' = doInstruction computer x

main :: IO ()
main = do
  input <- readFile "input.txt"
  let instructions = parseInstructions (splitOn "\n" input)
  putStrLn "Part One Solution:"
  print $ solveOne (Computer {currentMask = Mask "", memory = Vector.empty}) instructions