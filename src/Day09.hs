module Day09 where

import Data.Char (ord)
import Data.List (sort)

data HeightMap = HeightMap [[Int]] Int Int

width :: HeightMap -> Int
width (HeightMap _ width _) = width

height :: HeightMap -> Int
height (HeightMap _ _ height) = height

parse :: String -> HeightMap
parse input = HeightMap hm width height
  where
    hm = map (map (\c -> ord c - ord '0')) $ lines input
    width = length $ head hm
    height = length hm

(!!!) :: HeightMap -> (Int, Int) -> Int
_ !!! (-1, _) = maxBound
_ !!! (_, -1) = maxBound
(HeightMap hm width height) !!! (x, y)
  | x >= width || y >= height = maxBound
  | otherwise = (hm !! y) !! x

getIfMin :: (Int, Int) -> HeightMap -> [(Int, Int)]
getIfMin (x, y) hm = [(x, y) | all (> c) ltrb]
  where
    c = hm !!! (x, y)
    ltrb = map (hm !!!) [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

findMins :: HeightMap -> [(Int, Int)]
findMins hm = recur (0, 0) hm
  where
    recur (x, y) hm
      | x >= width hm = recur (0, y + 1) hm
      | y >= height hm = []
      | otherwise = getIfMin (x, y) hm ++ recur (x + 1, y) hm

d09p1 :: String -> IO ()
d09p1 input = do
  let hm = parse input
  let mins = findMins hm
  let risks = map (\c -> hm !!! c + 1) mins
  let sumRisks = sum risks
  putStrLn ("Sum of all risk levels is " ++ show sumRisks)

mapBasin :: (Int, Int) -> HeightMap -> Int
mapBasin c hm = recur [c] [] hm
  where
    recur [] _ _ = 0
    recur (c : cs) seen hm = 1 + recur (cs ++ ncs) (seen ++ (c : ncs)) hm
      where
        basinFilter (c, v) = v < 9 && v > cv && notElem c seen
        ncs = map fst . filter basinFilter $ map (\cn -> (cn, hm !!! cn)) ltrb
        ltrb = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
        cv = hm !!! c
        (x, y) = c

d09p2 :: String -> IO ()
d09p2 input = do
  let hm = parse input
  let mins = findMins hm
  let basins = map (`mapBasin` hm) mins
  let topThreeBasins = take 3 . reverse $ sort basins
  let result = product topThreeBasins
  putStrLn ("Product of the sizes of the three largest basins is " ++ show result)
