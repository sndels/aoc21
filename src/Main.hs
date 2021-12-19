import Day00 ( d00p1, d00p2 )
import Day01 ( d01p1, d01p2 )
import Day02 ( d02p1, d02p2 )
import Day03 ( d03p1, d03p2 )
import Day04 ( d04p1, d04p2 )
import Day05 ( d05p1, d05p2 )
import Day06 ( d06p1, d06p2 )
import Day07 ( d07p1, d07p2 )
import Day08 ( d08p1, d08p2 )
import Day09 ( d09p1, d09p2 )
import Day10 ( d10p1, d10p2 )
import Day11 ( d11p1, d11p2 )
import Day12 ( d12p1, d12p2 )
import Day13 ( d13p1, d13p2 )
import Day14 ( d14p1, d14p2 )
import Day15 ( d15p1, d15p2 )
import Day16 ( d16p1, d16p2 )
import Day17 ( d17p1, d17p2 )
import Day18 ( d18p1, d18p2 )
import Day19 ( d19p1, d19p2 )
import System.Directory ( doesFileExist )
import System.Environment ( getArgs )

run day part inputPath = do
  input <- readFile inputPath
  case day of
    0 -> case part of
      1 -> d00p1 input
      2 -> d00p2 input
    1 -> case part of
      1 -> d01p1 input
      2 -> d01p2 input
    2 -> case part of
      1 -> d02p1 input
      2 -> d02p2 input
    3 -> case part of
      1 -> d03p1 input
      2 -> d03p2 input
    4 -> case part of
      1 -> d04p1 input
      2 -> d04p2 input
    5 -> case part of
      1 -> d05p1 input
      2 -> d05p2 input
    6 -> case part of
      1 -> d06p1 input
      2 -> d06p2 input
    7 -> case part of
      1 -> d07p1 input
      2 -> d07p2 input
    8 -> case part of
      1 -> d08p1 input
      2 -> d08p2 input
    9 -> case part of
      1 -> d09p1 input
      2 -> d09p2 input
    10 -> case part of
      1 -> d10p1 input
      2 -> d10p2 input
    11 -> case part of
      1 -> d11p1 input
      2 -> d11p2 input
    12 -> case part of
      1 -> d12p1 input
      2 -> d12p2 input
    13 -> case part of
      1 -> d13p1 input
      2 -> d13p2 input
    14 -> case part of
      1 -> d14p1 input
      2 -> d14p2 input
    15 -> case part of
      1 -> d15p1 input
      2 -> d15p2 input
    16 -> case part of
      1 -> d16p1 input
      2 -> d16p2 input
    17 -> case part of
      1 -> d17p1 input
      2 -> d17p2 input
    18 -> case part of
      1 -> d18p1 input
      2 -> d18p2 input
    19 -> case part of
      1 -> d19p1 input
      2 -> d19p2 input
    d -> putStrLn $ "Day '" ++ show d ++ "' is not implemented"

main = do
  args <- getArgs
  let day = read $ head args :: Integer
  let part = read $ args !! 1 :: Integer
  let inputPath =
        if day < 10
          then "res/day0" ++ show day ++ ".txt"
          else "res/day" ++ show day ++ ".txt"
  inputExists <- doesFileExist inputPath
  if inputExists
    then run day part inputPath
    else putStrLn $ "Missing input for day " ++ show day
