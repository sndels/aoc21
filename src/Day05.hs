module Day05 where

import Data.Foldable (Foldable (foldl'))
import qualified Data.Map.Strict as Map
import Text.Regex.TDFA ((=~))

data Line = Line (Int, Int) (Int, Int) deriving (Show)

-- Sort points to avoid permutations later
line :: [Int] -> Line
line [x0, y0, x1, y1]
  | x0 < x1 = Line (x0, y0) (x1, y1)
  | x0 == x1 && y0 < y1 = Line (x0, y0) (x1, y1)
  | otherwise = Line (x1, y1) (x0, y0)
line _ = error "Invalid line constructor, expected list of 4 Ints"

maxCoord :: Line -> (Int, Int)
maxCoord (Line (x0, y0) (x1, y1)) = (max x0 x1, max y0 y1)

isHorizontal :: Line -> Bool
isHorizontal (Line (_, y0) (_, y1)) = y0 == y1

isVertical :: Line -> Bool
isVertical (Line (x0, _) (x1, _)) = x0 == x1

isDiagonal :: Line -> Bool
isDiagonal (Line (x0, y0) (x1, y1)) = (abs x1 - x0) == abs (max y0 y1 - min y0 y1)

parseLine :: String -> Line
parseLine l = line $ map read coords
  where
    (_, _, _, coords) = l =~ "([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)" :: (String, String, String, [String])

parse :: String -> [Line]
parse = map parseLine . lines

incrementOrAddOne :: (Int, Int) -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
incrementOrAddOne (x, y) lineMap =
  if Map.member (x, y) lineMap
    then Map.adjust (+ 1) (x, y) lineMap
    else Map.insert (x, y) 1 lineMap

addVertical :: Line -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
addVertical (Line (x, y0) (_, y1)) lineMap = foldl' (\m y -> incrementOrAddOne (x, y) m) lineMap [y0 .. y1]

addHorizontal :: Line -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
addHorizontal (Line (x0, y) (x1, _)) lineMap = foldl' (\m x -> incrementOrAddOne (x, y) m) lineMap [x0 .. x1]

addDiagonal :: Line -> Map.Map (Int, Int) Int -> Map.Map (Int, Int) Int
addDiagonal (Line (x0, y0) (x1, y1)) lineMap = foldl' (\m (x, y) -> incrementOrAddOne (x, y) m) lineMap $ zip [x0 .. x1] ys
  where
    ys = if y0 < y1 then [y0, y0 + 1 ..] else [y0, y0 - 1 ..]

generateMap :: Bool -> [Line] -> Map.Map (Int, Int) Int
generateMap addDiagonals = foldl' addLine Map.empty
  where
    addLine lineMap line
      | isHorizontal line = addHorizontal line lineMap
      | isVertical line = addVertical line lineMap
      | isDiagonal line = if addDiagonals then addDiagonal line lineMap else lineMap
      | otherwise = lineMap

d05p1 :: String -> IO ()
d05p1 input = do
  let lines = parse input
  let map = generateMap False lines
  let dangerousPoints = Map.foldl' (\a p -> a + fromEnum (p > 1)) 0 map
  putStrLn ("Found " ++ show dangerousPoints ++ " dangerous points")

d05p2 :: String -> IO ()
d05p2 input = do
  let lines = parse input
  let map = generateMap True lines
  let dangerousPoints = Map.foldl' (\a p -> a + fromEnum (p > 1)) 0 map
  putStrLn ("Found " ++ show dangerousPoints ++ " dangerous points")
