module Day03 where

import Data.Bits
import Data.Foldable

parse :: String -> [[Int]]
parse = map stringToBits . lines
  where
    stringToBits = map (read . charToString)
    charToString c = [c]

findGamma :: Foldable t => t [Int] -> [Int]
findGamma report = map oneIsMoreCommon $ foldl1 sumBits report
  where
    sumBits = zipWith (+)
    -- Needs to be at least as large as the "larger" half
    oneIsMoreCommon count = count `div` half
    half = (length report - 1) `div` 2 + 1

gammaToEpsilon :: [Int] -> [Int]
gammaToEpsilon = map (fromEnum . not . (==) 1)

bitsToInt :: [Int] -> Int
bitsToInt = foldl1 appendBits
  where
    appendBits a bit = a `shiftL` 1 .|. bit

d03p1 :: String -> IO ()
d03p1 input = do
  let report = parse input
  let gammaBits = findGamma report
  let gamma = bitsToInt gammaBits
  let epsilon = bitsToInt $ gammaToEpsilon gammaBits
  putStrLn ("Gamma " ++ show gamma ++ " epsilon " ++ show epsilon)
  let power = gamma * epsilon
  putStrLn ("Power " ++ show power)

filterUntilOne :: ([[Int]] -> [Int]) -> [[Int]] -> [Int]
filterUntilOne maskGen = recur 0
  where
    recur _ [r] = r
    recur bit report = recur (bit + 1) filtered
      where
        maskBit = maskGen report !! bit
        filtered = filter (\r -> (r !! bit) == maskBit) report

d03p2 :: String -> IO ()
d03p2 input = do
  let report = parse input
  -- Gamma has a 1 if |1s| >= |0s| and 0 otherwise
  -- Epsilon is gamma's bitwise complement, so has a 1 if |1s| < |0s|
  -- These are exactly the filter bit criteria for the two ratings
  let oxygen = bitsToInt $ filterUntilOne findGamma report
  let co2 = bitsToInt $ filterUntilOne (gammaToEpsilon . findGamma) report
  putStrLn ("Oxygen generator rating " ++ show oxygen ++ " CO2 scrubber rating " ++ show co2)
  let lifeSupport = oxygen * co2
  putStrLn ("Life support rating " ++ show lifeSupport)
