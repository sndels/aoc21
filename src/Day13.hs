module Day13 where

import Data.Foldable (Foldable (foldl'))
import Data.List (nub)
import Text.Regex.TDFA ((=~))

data Dot = Dot {x :: Int, y :: Int} deriving (Eq, Show)

data Direction = X | Y deriving (Eq, Show)

data Fold = Fold {dir :: Direction, pos :: Int} deriving (Show)

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x, y) = span (/= d) s

parseDot :: String -> Dot
parseDot dot = Dot x y
  where
    [x, y] = map read $ split ',' dot

parseFold :: String -> Fold
parseFold fold = if dir == "x" then Fold X (read pos) else Fold Y (read pos)
  where
    (_, _, _, [dir, pos]) = fold =~ "fold along ([xy])=([0-9]+)" :: (String, String, String, [String])

parse :: String -> ([Dot], [Fold])
parse input = (map parseDot dots, map parseFold folds)
  where
    [dots, folds] = split [] $ lines input

foldX :: Int -> Dot -> Dot
foldX pos (Dot x y) = if x > pos then Dot (2 * pos - x) y else Dot x y

foldY :: Int -> Dot -> Dot
foldY pos (Dot x y) = if y >= pos then Dot x (2 * pos - y) else Dot x y

foldDots :: Fold -> [Dot] -> [Dot]
foldDots (Fold dir pos) dots = nub $ map fold dots
  where
    fold = if dir == X then foldX pos else foldY pos

d13p1 :: String -> IO ()
d13p1 input = do
  let (dots, folds) = parse input
  let folded = foldDots (head folds) dots
  let foldedDots = length folded
  putStrLn (show foldedDots ++ " dots visible after first fold")

showDot :: Dot -> [Dot] -> Char
showDot (Dot x y) dots = if Dot x y `elem` dots then '#' else '.'

showDotLine :: Int -> [Dot] -> String
showDotLine y dots = recur 0 y dots
  where
    recur x y dots
      | x > 44 = "\n"
      | otherwise = showDot (Dot x y) dots : recur (x + 1) y dots

showDots :: [Dot] -> String
showDots dots = recur 0 dots
  where
    recur y dots
      | y > 8 = []
      | otherwise = showDotLine y dots ++ recur (y + 1) dots

d13p2 :: String -> IO ()
d13p2 input = do
  let (dots, folds) = parse input
  let folded = foldl' (flip foldDots) dots folds
  putStrLn (showDots folded)
