module Day02 where

import Data.Foldable

data Direction = Up | Down | Forward deriving (Eq, Enum)

direction d
  | d == "up" = Up
  | d == "down" = Down
  | d == "forward" = Forward

data Move = Move Direction Int

move [dir, amount] = Move (direction dir) (read amount :: Int)

parse input = map (move . words) $ lines input

makeSimpleMove (pos, depth) (Move dir amount) =
  case dir of
    Up -> (pos, depth - amount)
    Down -> (pos, depth + amount)
    Forward -> (pos + amount, depth)

makeSlightlyMoreComplicatedMove (pos, aim, depth) (Move dir amount) =
  case dir of
    Up -> (pos, aim - amount, depth)
    Down -> (pos, aim + amount, depth)
    Forward -> (pos + amount, aim, depth + (aim * amount))

d02p1 :: String -> IO ()
d02p1 input = do
  let course = parse input
  let (pos, depth) = foldl' makeSimpleMove (0, 0) course
  putStrLn ("Final position " ++ show pos ++ ", depth " ++ show depth)
  let result = pos * depth
  putStrLn ("position * depth = " ++ show result)

d02p2 :: String -> IO ()
d02p2 input = do
  let course = parse input
  let (pos, aim, depth) = foldl' makeSlightlyMoreComplicatedMove (0, 0, 0) course
  putStrLn ("Final aim " ++ show aim ++ ", position " ++ show pos ++ ", depth " ++ show depth)
  let result = pos * depth
  putStrLn ("position * depth = " ++ show result)
