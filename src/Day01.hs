module Day01 where

parse :: String -> [Int]
parse input = map read $ lines input

countIncreases (x : xs) = recur x xs
  where
    recur prev [] = 0
    recur prev (x : xs) = fromEnum (x > prev) + recur x xs

countWindowIncreases (x : y : z : w : xs) = recur [x, y, z] [y, z, w] xs
  where
    recur w0 w1 [] = increased w0 w1
    recur w0 w1 (x : xs) = increased w0 w1 + recur (tail w0 ++ [last w1]) (tail w1 ++ [x]) xs
    increased w0 w1 = fromEnum (sum w1 > sum w0)

d01p1 :: String -> IO ()
d01p1 input = do
  let measurements = parse input
  let incCount = countIncreases measurements
  putStrLn ("Found " ++ show incCount ++ " increases")

d01p2 :: String -> IO ()
d01p2 input = do
  let measurements = parse input
  let incCount = countWindowIncreases measurements
  putStrLn ("Found " ++ show incCount ++ " increases")
