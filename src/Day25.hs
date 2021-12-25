module Day25 where

import qualified Data.Map.Strict as Map
import Debug.Trace (trace)

data Floor = Floor {cucumbers :: Map.Map (Int, Int) Char, w :: Int, h :: Int}

instance Show Floor where
  show (Floor cucumbers w h) = recur 0 0
    where
      recur x y
        | x >= w = '\n' : recur 0 (y + 1)
        | y >= h = ""
        | otherwise = case cucumbers Map.!? (x, y) of
          Just v -> v : recur (x + 1) y
          Nothing -> '.' : recur (x + 1) y

parse :: [Char] -> Floor
parse input = Floor cucumbers w h
  where
    cucumbers = Map.fromList . foldl1 (++) $ zipWith parseLine [0 ..] ls
    parseLine y l = map (\(x, v) -> ((x, y), v)) . filter (\(x, v) -> v /= '.') $ zip [0 ..] l
    w = length $ head ls
    h = length ls
    ls = lines input

canMoveLeft cucus w h ((x, y), v)
  | x == w - 1 = not $ Map.member (0, y) cucus
  | otherwise = not $ Map.member (x + 1, y) cucus

canMoveDown cucus w h ((x, y), v)
  | y == h - 1 = not $ Map.member (x, 0) cucus
  | otherwise = not $ Map.member (x, y + 1) cucus

moveLeft w h ((x, y), v) cucus
  | x == w - 1 = Map.insert (0, y) '>' popd
  | otherwise = Map.insert (x + 1, y) '>' popd
  where
    popd = Map.delete (x, y) cucus

moveDown w h ((x, y), v) cucus
  | y == h - 1 = Map.insert (x, 0) 'v' popd
  | otherwise = Map.insert (x, y + 1) 'v' popd
  where
    popd = Map.delete (x, y) cucus

step :: Floor -> Maybe Floor
step floor = if moved then Just (Floor downMoved w h) else Nothing
  where
    moved = not (null downs) || not (null lefts)
    downMoved = foldl (\acc m -> m acc) leftMoved downs
    downs = map (moveDown w h) . filter (canMoveDown leftMoved w h) . filter ((== 'v') . snd) $ Map.toList leftMoved
    leftMoved = foldl (\acc m -> m acc) cucus lefts
    lefts = map (moveLeft w h) . filter (canMoveLeft cucus w h) . filter ((== '>') . snd) $ Map.toList cucus
    (Floor cucus w h) = floor

run :: Floor -> (Floor, Int)
run = recur 1
  where
    recur n floor = case step floor of
      Just newFloor -> trace ("Step " ++ show n) recur (n + 1) newFloor
      Nothing -> (floor, n)

d25p1 :: String -> IO ()
d25p1 input = do
  let floor = parse input
  let (stopped, n) = run floor
  print stopped
  putStrLn ("Sea cucumbers stopped after " ++ show n ++ " steps")
