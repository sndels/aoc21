module Day08 where

import Data.List as List (sort, (\\))
import qualified Data.Map.Strict as Map

-- Dear lord, how much spaghetti this is...

split' :: Eq a => a -> [a] -> [[a]]
split' d [] = []
split' d s = x : split' d (drop 1 y) where (x, y) = span (/= d) s

parseSegs :: String -> [String]
parseSegs = map sort . filter (not . null) . split' ' '

parse :: String -> [([String], [String])]
parse input = zip signals outputs
  where
    signals = map parseSegs rawSignals
    outputs = map parseSegs rawOutputs
    (rawSignals, rawOutputs) = unzip . map ((\[x, y] -> (x, y)) . split' '|') $ lines input

isOne :: String -> Bool
isOne segs = length segs == 2

isFour :: String -> Bool
isFour segs = length segs == 4

isSeven :: String -> Bool
isSeven segs = length segs == 3

isEight :: String -> Bool
isEight segs = length segs == 7

countOf :: (String -> Bool) -> [([String], [String])] -> Int
countOf cond entries = sum $ map (\(_, outputs) -> single outputs) entries
  where
    single = sum . map (fromEnum . cond)

d08p1 :: String -> IO ()
d08p1 input = do
  let entries = parse input
  let ones = countOf isOne entries
  let fours = countOf isFour entries
  let sevens = countOf isSeven entries
  let eights = countOf isEight entries
  let total = ones + fours + sevens + eights
  putStrLn (show total ++ " total")

data Segment = Top | TopLeft | TopRight | Middle | BottomLeft | BottomRight | Bottom

isThree :: String -> String -> Bool
isThree oneSegs segs = length (segs \\ oneSegs) == 3

isNine :: String -> String -> Bool
isNine threeSegs segs = length segs == 6 && length (segs \\ threeSegs) == 1

isFive :: String -> String -> Bool
isFive nineSegs segs = length segs == 5 && null (segs \\ nineSegs)

isTwo :: String -> String -> Bool
isTwo nineSegs segs = length segs == 5 && length (segs \\ nineSegs) == 1

isSix :: String -> String -> Bool
isSix fiveSegs segs = length segs == 6 && length (segs \\ fiveSegs) == 1

findNumbers :: [String] -> Map.Map Int String
findNumbers segs = recur 0 segs Map.empty
  where
    recur :: Int -> [String] -> Map.Map Int String -> Map.Map Int String
    recur 0 segs nums = recur 3 restSegs newNums
      where
        restSegs = segs \\ [one, four, seven, eight]
        newNums = Map.fromList [(8, eight), (7, seven), (4, four), (1, one)]
        one = head $ filter isOne segs
        four = head $ filter isFour segs
        seven = head $ filter isSeven segs
        eight = head $ filter isEight segs
    recur 3 segs nums = recur 9 restSegs newNums
      where
        restSegs = segs \\ [three]
        newNums = Map.insert 3 three nums
        three = head $ filter (isThree (nums Map.! 1)) segs
    recur 9 segs nums = recur 5 restSegs newNums
      where
        restSegs = segs \\ [nine]
        newNums = Map.insert 9 nine nums
        nine = head $ filter (isNine (nums Map.! 3)) segs
    recur 5 segs nums = recur 6 restSegs newNums
      where
        restSegs = segs \\ [two, five]
        newNums = Map.union nums $ Map.fromList [(2, two), (5, five)]
        five = head $ filter (isFive (nums Map.! 9)) segs
        two = head $ filter (isTwo (nums Map.! 9)) segs
    recur 6 segs nums = newNums
      where
        newNums = Map.union nums $ Map.fromList [(0, zero), (6, six)]
        zero = head $ segs \\ [six]
        six = head $ filter (isSix (nums Map.! 5)) segs
    recur _ _ _ = error "Invalid findNumbers recur call"

intoNumber :: (Map.Map String Int, [String]) -> Int
intoNumber (nums, [x0, x1, x2, x3]) = nums Map.! x0 * 1000 + nums Map.! x1 * 100 + nums Map.! x2 * 10 + nums Map.! x3
intoNumber _ = error "Invalid call to intoNumber"

d08p2 :: String -> IO ()
d08p2 input = do
  let entries = parse input
  let mapped = map (\(signals, outputs) -> (Map.fromList . map (\(a, b) -> (b, a)) . Map.toList $ findNumbers signals, outputs)) entries
  let numbers = map intoNumber mapped
  let outputSum = sum numbers
  putStrLn ("Sum of all outputs is " ++ show outputSum)
