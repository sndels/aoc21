module Day04 where

import Data.Foldable
import Data.List

data Board = Board [Int] [Int] deriving (Show)

board :: [Int] -> Board
board numbers = Board numbers []

mark :: Int -> Board -> Board
mark n (Board numbers marked) = f (elemIndex n numbers)
  where
    f (Just i) = Board numbers (marked ++ [i])
    f Nothing = Board numbers marked

bingoPermutations :: [[Int]]
bingoPermutations = rows ++ columns
  where
    rows = [[i, i + 1, i + 2, i + 3, i + 4] | i <- [0, 5 .. 20]]
    columns = [[i, i + 5, i + 10, i + 15, i + 20] | i <- [0 .. 4]]

countBingos :: Board -> Int
countBingos (Board _ marked) = length $ filter allMarked bingoPermutations
  where
    allMarked = foldl' (\a x -> a && x `elem` marked) True

unmarkedNumbers :: Board -> [Int]
unmarkedNumbers (Board numbers marked) = foldl' (flip delete) numbers $ map (numbers !!) marked

winners :: [Board] -> [Board]
winners = filter (\b -> countBingos b > 0)

notYetWinners :: [Board] -> [Board]
notYetWinners = filter (\b -> countBingos b == 0)

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x, y) = span (/= d) s

parseNumbers :: String -> [Int]
parseNumbers = map read . split ','

parseBoards :: [String] -> [Board]
parseBoards = map (parseBoard . joinLines) . group 5
  where
    group _ [] = []
    group n l = take n l : group n (drop n l)
    joinLines = foldl1 (\a r -> a ++ " " ++ r)

parseBoard :: String -> Board
parseBoard = board . map read . filter (not . null) . split ' '

parse :: String -> ([Int], [Board])
parse input = (parseNumbers $ head chunks, parseBoards $ tail chunks)
  where
    chunks = filter (not . null) $ lines input

findFirstWinner :: [Int] -> [Board] -> (Int, Board)
findFirstWinner numbers boards = recur (-1, []) numbers boards
  where
    recur (n, [b]) _ _ = (n, b)
    recur (_, []) (n : ns) boards = recur (n, winners markedBoards) ns markedBoards
      where
        markedBoards = map (mark n) boards
    recur _ _ _ = error "Multiple winners"

d04p1 :: String -> IO ()
d04p1 input = do
  let (numbers, boards) = parse input
  let (winningNumber, winner) = findFirstWinner numbers boards
  let sumUnmarked = sum $ unmarkedNumbers winner
  let score = winningNumber * sumUnmarked
  putStrLn ("Final score of the winning board would be " ++ show score)

findLastWinner :: [Int] -> [Board] -> (Int, Board)
findLastWinner numbers boards = recur (-1, []) numbers boards
  where
    recur (n, prevWinners) _ [] = (n, head prevWinners)
    recur (_, prevWinners) (n : ns) boards = recur (n, winners markedBoards ++ prevWinners) ns (notYetWinners markedBoards)
      where
        markedBoards = map (mark n) boards

d04p2 :: String -> IO ()
d04p2 input = do
  let (numbers, boards) = parse input
  let (winningNumber, winner) = findLastWinner numbers boards
  let sumUnmarked = sum $ unmarkedNumbers winner
  let score = winningNumber * sumUnmarked
  putStrLn ("Final score of the last winning board would be " ++ show score)
