module Day14 where

import Data.Foldable (Foldable (foldl'))
import Data.List (foldl', nub, sort)
import qualified Data.Map as Map
import Text.Regex.TDFA ((=~))

type Rules = Map.Map (Char, Char) Char

parseRule :: String -> ((Char, Char), Char)
parseRule rule = ((head pair, last pair), head insertion)
  where
    (_, _, _, [pair, insertion]) = rule =~ "([A-Z][A-Z]) -> ([A-Z])" :: (String, String, String, [String])

parse :: [Char] -> (String, Rules)
parse input = (polymer, Map.fromList rules)
  where
    polymer = head ls
    rules = map parseRule . filter (not . null) $ tail ls
    ls = lines input

insertElements :: Rules -> [Char] -> [Char]
insertElements rules (e0 : e1 : es) = e0 : rules Map.! (e0, e1) : insertElements rules (e1 : es)
insertElements rules [e] = [e]
insertElements _ _ = error "Invalid insertElements call"

countElements :: [Char] -> Map.Map Char Int
countElements polymer = foldl' (flip (Map.adjust (+ 1))) emptyCounts polymer
  where
    emptyCounts = Map.fromList $ zip (nub polymer) (repeat 0)

d14p1 :: String -> IO ()
d14p1 input = do
  let (polymer, rules) = parse input
  let np = foldl' (\p _ -> insertElements rules p) polymer [0 .. 9]
  let counts = sort . map snd . Map.toList $ countElements np
  print counts
  let result = last counts - head counts
  putStrLn ("Difference of the quantities is " ++ show result)

intoPairs :: [Char] -> [(Char, Char)]
intoPairs [e] = [(e, ' ')]
intoPairs (e0 : e1 : es) = (e0, e1) : intoPairs (e1 : es)
intoPairs _ = error "Invalid intoPairs call"

pairCounts :: [(Char, Char)] -> Map.Map (Char, Char) Int
pairCounts = foldl' (\counts p -> Map.insertWith (+) p 1 counts) Map.empty

type CompactPoly = Map.Map (Char, Char) Int

insertCompactElement :: Rules -> (Char, Char) -> Int -> CompactPoly -> CompactPoly
insertCompactElement rules (c, ' ') _ poly = Map.insert (c, ' ') 1 poly
insertCompactElement rules (e0, e1) count poly = foldl' (\ps p -> Map.insertWith (+) p count ps) poly newPairs
  where
    newPairs = [(e0, ei), (ei, e1)]
    ei = rules Map.! (e0, e1)

insertCompactElements :: Rules -> CompactPoly -> CompactPoly
insertCompactElements rules =
  Map.foldlWithKey (\new pair count -> insertCompactElement rules pair count new) Map.empty

countCompactElements :: CompactPoly -> Map.Map Char Int
countCompactElements = Map.foldlWithKey (\counts (e, _) count -> Map.insertWith (+) e count counts) Map.empty

d14p2 :: String -> IO ()
d14p2 input = do
  let (polymer, rules) = parse input
  let compactPolymer = pairCounts $ intoPairs polymer
  let np = foldl' (\p _ -> insertCompactElements rules p) compactPolymer [0 .. 39]
  let counts = sort . map snd . Map.toList $ countCompactElements np
  let result = last counts - head counts
  putStrLn ("Difference of the quantities is " ++ show result)
