module Day12 where

import Data.Char ( isUpper )
import Data.List ( nub )
import qualified Data.Map.Strict as Map

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x, y) = span (/= d) s

type Caves = Map.Map String [String]

addConnection :: String -> String -> Caves -> Caves
addConnection a b = Map.insertWith (++) a [b]

collectSystem :: [[String]] -> Caves
collectSystem connections = recur connections Map.empty
  where
    recur [] caves = caves
    recur ([a, b] : cs) caves = recur cs . addConnection b a $ addConnection a b caves
    recur (c : cs) _ = error ("Invalid collectSystem call with c = " ++ show c)

parse :: String -> Caves
parse = collectSystem . map (split '-') . lines

isBig :: String -> Bool
isBig = all isUpper

pathsToEnd :: (String -> [String] -> Bool) -> Caves -> [[String]]
pathsToEnd traverseCond caves = recur "start" [] [] caves
  where
    recur cave path bfs caves
      | cave == "end" = reverse ("end" : path) : popNext bfs caves
      | traverseCond cave path = traverse cave path bfs caves
      | otherwise = popNext bfs caves
    popNext ((c : cs) : bfs) caves = recur c cs bfs caves
    popNext [] _ = []
    popNext (cs : _) caves = error ("Inivalid popNext cs = " ++ show cs)
    traverse cave path bfs caves = recur nextCave newPath newBfs caves
      where
        newBfs = map (: newPath) (tail neighbours) ++ bfs
        nextCave = head neighbours
        neighbours = caves Map.! cave
        newPath = cave : path

singleVisit :: String -> [String] -> Bool
singleVisit cave path = isBig cave || cave `notElem` path

d12p1 :: String -> IO ()
d12p1 input = do
  let caves = parse input
  let paths = pathsToEnd singleVisit caves
  putStrLn ("Found " ++ show (length paths) ++ " paths from 'start' to 'end'")

doubleVisit :: String -> [String] -> Bool
doubleVisit cave path = isBig cave || (notVisited || (notRevisitingStart && noTwiceVisited))
  where
    notVisited = cave `notElem` path
    noTwiceVisited = length (nub onlySmall) == length onlySmall
    onlySmall = filter (not . isBig) path
    notRevisitingStart = null path || cave /= "start"

d12p2 :: String -> IO ()
d12p2 input = do
  let caves = parse input
  let paths = pathsToEnd doubleVisit caves
  putStrLn ("Found " ++ show (length paths) ++ " paths from 'start' to 'end'")
