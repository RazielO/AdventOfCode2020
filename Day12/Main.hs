module Main where

import Data.List.Split (splitOn)

data Instruction = Instruction
  { action :: Char,
    value :: Int
  }
  deriving (Show)

data Ship = Ship
  { course :: Int,
    position :: (Int, Int),
    waypoint :: (Int, Int)
  }
  deriving (Show)

toRadians :: Int -> Double
toRadians theta = fromIntegral theta / 180 * pi

manhattanDistance :: Ship -> Int
manhattanDistance (Ship course (x, y) w) = abs x + abs y

moveShip :: Ship -> Char -> Int -> Ship
moveShip ship@(Ship course (x, y) w) direction value
  | direction == 'N' = Ship course (x + value, y) w
  | direction == 'S' = Ship course (x - value, y) w
  | direction == 'E' = Ship course (x, y + value) w
  | direction == 'W' = Ship course (x, y - value) w
  | otherwise = ship

doInstruction :: Ship -> Instruction -> Ship
doInstruction ship@(Ship course (x, y) w) inst
  | action inst `elem` ['N', 'S', 'E', 'W'] = moveShip ship (action inst) (value inst)
  | action inst == 'L' = Ship (course + value inst) (x, y) w
  | action inst == 'R' = Ship (course - value inst) (x, y) w
  | action inst == 'F' = moveShip ship direction (value inst)
  where
    direction
      | course `mod` 360 == 0 = 'E'
      | course `mod` 360 == 90 = 'N'
      | course `mod` 360 == 180 = 'W'
      | course `mod` 360 == 270 = 'S'
      | otherwise = 'X'

rotateWaypoint :: Ship -> Char -> Int -> Ship
rotateWaypoint ship@(Ship course (px, py) (wx, wy)) direction value = ship'
  where
    ship' = Ship course (px, py) (wx', wy')
    wx' = round $ fromIntegral wx * cos angle - fromIntegral wy * sin angle
    wy' = round $ fromIntegral wy * cos angle + fromIntegral wx * sin angle
    angle
      | direction == 'R' = toRadians (-1 * value)
      | otherwise = toRadians value

instructionWaypoint :: Ship -> Instruction -> Ship
instructionWaypoint ship@(Ship course (px, py) (wx, wy)) inst
  | act == 'N' = Ship course (px, py) (wx, wy + val)
  | act == 'S' = Ship course (px, py) (wx, wy - val)
  | act == 'E' = Ship course (px, py) (wx + val, wy)
  | act == 'W' = Ship course (px, py) (wx - val, wy)
  | act == 'F' = Ship course (px + (wx * val), py + (wy * val)) (wx, wy)
  | act `elem` ['L', 'R'] = rotateWaypoint ship act val
  where
    act = action inst
    val = value inst

doInstructions :: Ship -> [Instruction] -> (Ship -> Instruction -> Ship) -> Ship
doInstructions ship [] _ = ship
doInstructions ship (x : xs) func = doInstructions ship' xs func
  where
    ship' = func ship x

main :: IO ()
main = do
  input <- readFile "input.txt"
  let instructions = map (\x -> Instruction {action = head x, value = read (tail x) :: Int}) (splitOn "\n" input)
  putStrLn "Part One Solution:"
  print $ manhattanDistance $ doInstructions (Ship 0 (0, 0) (0, 0)) instructions doInstruction
  putStrLn "Part Two Solution:"
  print $ manhattanDistance $ doInstructions (Ship 0 (0, 0) (10, 1)) instructions instructionWaypoint