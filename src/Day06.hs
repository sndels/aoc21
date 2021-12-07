module Day06 where

import Debug.Trace

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x, y) = span (/= d) s

parse :: String -> [Int]
parse = map read . split ','

-- This is a naive yolo
step :: [Int] -> [Int]
step = recur 0
  where
    recur newFish [] = replicate newFish 8
    recur newFish (f : fs) =
      if f == 0
        then 6 : recur (newFish + 1) fs
        else f - 1 : recur newFish fs

simulate :: Int -> [Int] -> [Int]
simulate days fish = recur days (length fish) fish
  where
    recur 0 _ fish = fish
    recur days prevCount fish = trace ("Day " ++ show days ++ " fish " ++ show (length fish) ++ " diff " ++ show (prevCount - length fish)) recur (days - 1) (length fish) $ step fish

d06p1 :: String -> IO ()
d06p1 input = do
  let startFish = parse input
  let endFish = simulate 80 startFish
  putStrLn (show (length endFish) ++ " at 80 days")

compact :: [Int] -> [Int]
compact fish = map (\v -> length $ filter (== v) fish) [0 .. 8]

simulateCompact :: Int -> [Int] -> [Int]
simulateCompact 0 fish = fish
simulateCompact days [x0, x1, x2, x3, x4, x5, x6, x7, x8] = simulateCompact (days - 1) [x1, x2, x3, x4, x5, x6, x7 + x0, x8, x0]
simulateCompact _ _ = error "Invalid input, expected 9 fish pools"

d06p2 :: String -> IO ()
d06p2 input = do
  let startFish = parse input
  let compactFish = compact startFish
  let endFish = simulateCompact 256 $ compact startFish
  putStrLn (show (sum endFish) ++ " at 256 days")
