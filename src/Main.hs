import Day00
import Day01
import Day02
import System.Directory
import System.Environment
import System.IO

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
