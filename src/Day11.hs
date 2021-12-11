module Day11 where

import Data.Char ( ord )
import qualified Data.Map.Strict as Map

type Coord = (Int, Int)

type Octos = Map.Map Coord Int

parse :: String -> Octos
parse input = Map.fromList flatValues
  where
    flatValues = foldl1 (++) $ map (\(y, r) -> map (\(x, v) -> ((x, y), v)) r) enumValues
    enumValues = zip [0 ..] $ map (zip [0 ..]) values
    values = map (map (\c -> ord c - ord '0')) $ lines input

step :: Octos -> (Int, Octos)
step octos = (flashes, reset flashed)
  where
    (flashes, flashed) = flashAll stepped
    stepped = Map.map incr octos

runSteps :: Int -> Octos -> (Int, Octos)
runSteps steps octos = recur steps (0, octos)
  where
    recur 0 acc = acc
    recur n (prevFlashes, octos) = recur (n - 1) (prevFlashes + flashes, newOctos)
      where
        (flashes, newOctos) = step octos

findSync :: Octos -> (Int, Octos)
findSync = recur 1
  where
    recur n octos =
      if flashes == Map.size octos
        then (n, newOctos)
        else recur (n + 1) newOctos
      where
        (flashes, newOctos) = step octos

incr :: Int -> Int
incr v = if v >= 0 then v + 1 else v

flash :: Octos -> Octos
flash = Map.map (\v -> if v > 9 then -1 else v)

propagateFlashes :: [Coord] -> Octos -> Octos
propagateFlashes [] octos = octos
propagateFlashes ((x, y) : fs) octos = propagateFlashes fs $ foldr (Map.adjust incr) octos affected
  where
    affected = filter (/= (x, y)) $ [(x + i, y + j) | i <- [-1 .. 1], j <- [-1 .. 1]]

flashAll :: Octos -> (Int, Octos)
flashAll octos = recur 0 octos
  where
    recur flashCount octos =
      if newFlashes > 0
        then recur (flashCount + newFlashes) propagated
        else (flashCount, propagated)
      where
        newFlashes = length flashing
        propagated = propagateFlashes flashing flashed
        flashed = flash octos
        flashing = Map.keys $ Map.filter (> 9) octos

reset :: Octos -> Octos
reset = Map.map (\v -> if v == -1 then 0 else v)

d11p1 :: String -> IO ()
d11p1 input = do
  let octos = parse input
  let (flashCount, finalOctos) = runSteps 100 octos
  putStrLn (show flashCount ++ " flashes after 100 steps")

d11p2 :: String -> IO ()
d11p2 input = do
  let octos = parse input
  let (syncStep, finalOctos) = findSync octos
  putStrLn ("First sychronized flash at step " ++ show syncStep)
