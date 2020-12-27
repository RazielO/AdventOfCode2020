module Main where

import Data.List.Split (splitOn)
import Debug.Trace (trace)

data Instruction = Instruction
  { action :: Char,
    value :: Int
  }
  deriving (Show)

data Ship = Ship
  { course :: Int,
    position :: (Int, Int)
  }
  deriving (Show)

moveShip :: Ship -> Char -> Int -> Ship
moveShip ship@(Ship course (x, y)) direction value
  | direction == 'N' = Ship course (x + value, y)
  | direction == 'S' = Ship course (x - value, y)
  | direction == 'E' = Ship course (x, y + value)
  | direction == 'W' = Ship course (x, y - value)
  | otherwise = ship

doInstruction :: Ship -> Instruction -> Ship
doInstruction ship@(Ship course (x, y)) inst
  | action inst `elem` ['N', 'S', 'E', 'W'] = moveShip ship (action inst) (value inst)
  | action inst == 'L' = Ship (course + value inst) (x, y)
  | action inst == 'R' = Ship (course - value inst) (x, y)
  | action inst == 'F' = moveShip ship direction (value inst)
  where
    direction
      | course `mod` 360 == 0 = 'E'
      | course `mod` 360 == 90 = 'N'
      | course `mod` 360 == 180 = 'W'
      | course `mod` 360 == 270 = 'S'
      | otherwise = 'X'

doInstructions :: Ship -> [Instruction] -> Ship
doInstructions ship [] = ship
doInstructions ship (x : xs) = doInstructions ship' xs
  where
    ship' = doInstruction ship x

manhattanDistance :: Ship -> Int
manhattanDistance (Ship course (x, y)) = abs x + abs y

main :: IO ()
main = do
  input <- readFile "input.txt"
  let instructions = map (\x -> Instruction {action = head x, value = read (tail x) :: Int}) (splitOn "\n" input)
  print $ manhattanDistance $ doInstructions (Ship 0 (0, 0)) instructions