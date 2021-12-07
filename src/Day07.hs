module Day07 where

import Data.List (sort)

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x, y) = span (/= d) s

parse :: String -> [Int]
parse = map read . split ','

diff :: Int -> Int -> Int
diff p0 p1 = abs (p0 - p1)

fuelSpendToTarget :: (Int -> Int -> Int) -> Int -> [Int] -> Int
fuelSpendToTarget spendFn target positions = sum $ map (spendFn target) positions

d07p1 :: String -> IO ()
d07p1 input = do
  let positions = sort $ parse input
  let median = positions !! (length positions `div` 2)
  let fuelSpend = fuelSpendToTarget diff median positions
  putStrLn ("Aligning at " ++ show median ++ " takes " ++ show fuelSpend ++ " fuel")

data NaturalTree a = Node a (NaturalTree a) (NaturalTree a)

cost2Memo :: Int -> Int
cost2Memo i = cost2 0 0 10000 !! i
  where
    cost2 prevCost i n
      | i == n = [prevCost + n]
      | otherwise = newCost : cost2 newCost (i + 1) n
      where
        newCost = prevCost + i

fuelSpend2 :: Int -> [Int] -> Int
fuelSpend2 = fuelSpendToTarget (\target v -> cost2Memo $ diff target v)

descent :: (Int -> Int) -> [Int] -> Int -> (Int, Int)
descent iter positions target = recur (fuelSpend2 target positions) target
  where
    recur prevCost prevTarget =
      if newCost < prevCost
        then recur newCost newTarget
        else (prevCost, prevTarget)
      where
        newCost = fuelSpend2 newTarget positions
        newTarget = iter prevTarget

d07p2 :: String -> IO ()
d07p2 input = do
  let positions = sort $ parse input
  let median = positions !! (length positions `div` 2)
  let minPos = head positions
  let maxPos = last positions
  let (fuelSpend, target) =
        if median - minPos > maxPos - median
          then descent (1 -) positions median
          else descent (1 +) positions median
  putStrLn ("Aligning at " ++ show target ++ " takes " ++ show fuelSpend ++ " fuel")
