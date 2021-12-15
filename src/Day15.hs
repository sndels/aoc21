module Day15 where

import Data.Char (ord)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (trace)

type Coord = (Int, Int)

type Risks = Map.Map (Int, Int) Int

type Dist = Map.Map Coord Int

type Prev = Map.Map Coord Coord

-- This seems really slow and spaghetti'y
-- TODO: Clean up and optimize
--       Seems like a good place to learn new haskell stuff
--       Also, use a priority queue

parse :: String -> Risks
parse input = Map.fromList flatValues
  where
    flatValues = foldl1 (++) $ map (\(y, r) -> map (\(x, v) -> ((x, y), v)) r) enumValues
    enumValues = zip [0 ..] $ map (zip [0 ..]) values
    values = map (map (\c -> ord c - ord '0')) $ lines input

initQueue start = Set.insert start Set.empty

initDist :: Coord -> Risks -> Dist
initDist start = Map.insert start 0 . Map.fromList . flip zip (repeat maxBound) . Map.keys

minBy :: (Coord -> Coord -> Bool) -> Set.Set Coord -> Coord
minBy comp cs = recur comp $ Set.elems cs
  where
    recur comp [x] = x
    recur comp (a : b : xs) = if comp a b then recur comp (a : xs) else recur comp (b : xs)
    recur _ _ = error "Invalid call to minBy with empty list"

updateNeightbour :: Int -> Risks -> Coord -> Dist -> Dist
updateNeightbour d risks n dist = if newD < oldD then Map.insert n newD dist else dist
  where
    oldD = dist Map.! n
    newD = d + risks Map.! n

neighbours :: Coord -> Set.Set Coord -> Map.Map Coord Int -> Set.Set Coord
neighbours (x, y) visited risks = flip Set.difference visited . Set.fromList . filter (`Map.member` risks) $ [(x - 1, y), (x, y - 1), (x + 1, y), (x, y + 1)]

dijkstra :: Coord -> Coord -> Risks -> Dist
dijkstra start target risks = recur Set.empty (initQueue start) (initDist start risks)
  where
    recur visited queue dist
      | null queue = dist
      | otherwise =
        if fst coord `mod` 100 == 0 && snd coord `mod` 100 == 0
          then trace (show coord) recur newVisited newQueue newDist
          else recur newVisited newQueue newDist
      where
        newDist = foldr (updateNeightbour d risks) dist ns
        newVisited = Set.insert coord visited
        newQueue = Set.union ns $ Set.delete coord queue
        ns = neighbours coord visited risks
        d = dist Map.! coord
        coord = minBy (\a b -> dist Map.! a < dist Map.! b) queue

d15p1 :: String -> IO ()
d15p1 input = do
  let risks = parse input
  let target = (maximum . map fst $ Map.keys risks, maximum . map snd $ Map.keys risks)
  let risk = dijkstra (0, 0) target risks Map.! target
  putStrLn ("Lowest total risk is " ++ show risk)

mod9 :: Int -> Int
mod9 c = ((c - 1) `mod` 9) + 1

tileRisks :: Risks -> Risks
tileRisks risks = recur 0 0
  where
    recur x y
      | x == 5 = recur 0 (y + 1)
      | y == 5 = Map.empty
      | otherwise = Map.union tile $ recur (x + 1) y
      where
        tile = Map.fromList $ map (\((rx, ry), rv) -> ((xo + rx, yo + ry), mod9 (rv + x + y))) rs
        xo = x * w
        yo = y * h
    rs = Map.toList risks
    w = 1 + (maximum . map fst $ Map.keys risks)
    h = 1 + (maximum . map snd $ Map.keys risks)

d15p2 :: String -> IO ()
d15p2 input = do
  let risks = tileRisks $ parse input
  let cs = Map.keys risks
  let target = (maximum $ map fst cs, maximum $ map snd cs)
  let risk = dijkstra (0, 0) target risks Map.! target
  putStrLn ("Lowest total risk is " ++ show risk)
