{-# LANGUAGE TupleSections #-}

module Day20 where

import Data.Bits (Bits (shiftL, (.|.)))
import qualified Data.Map.Strict as Map

data Coord = Coord Int Int deriving (Eq, Show, Ord)

readPxs :: [[Char]] -> Map.Map Coord Char
readPxs input = Map.fromList $ recur 0 input
  where
    recur y [] = []
    recur y (l : ls) = ps ++ recur (y + 1) ls
      where
        ps = zipWith (\x p -> (Coord x y, p)) [0 ..] l

parse :: [Char] -> (String, Map.Map Coord Char)
parse input = (iea, pxs)
  where
    pxs = readPxs $ drop 2 ls
    iea = head ls
    ls = lines input

limits :: Map.Map Coord Char -> (Coord, Coord)
limits pxs = (minPx, maxPx)
  where
    minPx = Coord minX minY
    maxPx = Coord maxX maxY
    minX = minimum . map (\(Coord x _) -> x) $ Map.keys pxs
    maxX = maximum . map (\(Coord x _) -> x) $ Map.keys pxs
    minY = minimum . map (\(Coord _ y) -> y) $ Map.keys pxs
    maxY = maximum . map (\(Coord _ y) -> y) $ Map.keys pxs

getBit :: Coord -> Map.Map Coord Char -> Int
getBit c pxs = fromEnum ((pxs Map.! c) == '#')

dilate' :: Char -> Map.Map Coord Char -> Map.Map Coord Char
dilate' cp pxs = Map.union pxs . Map.fromList $ leftPs ++ topPs ++ rightPs ++ bottomPs
  where
    leftPs = map ((,cp) . Coord (minX - 1)) [minY - 1 .. maxY + 1]
    topPs = map ((,cp) . (`Coord` (minY - 1))) [minX - 1 .. maxX + 1]
    rightPs = map ((,cp) . Coord (maxX + 1)) [minY - 1 .. maxY + 1]
    bottomPs = map ((,cp) . (`Coord` (maxY + 1))) [minX - 1 .. maxX + 1]
    (Coord minX minY, Coord maxX maxY) = limits pxs

dilate :: Map.Map Coord Char -> Map.Map Coord Char
dilate pxs = dilate' dp pxs
  where
    dp = pxs Map.! fst (limits pxs)

enhancePx :: Coord -> Map.Map Coord Char -> [Char] -> (Coord, Char)
enhancePx (Coord x y) pxs iea = (Coord x y, iea !! ieaI)
  where
    ieaI = foldl1 (\acc b -> (acc `shiftL` 1) .|. b) bits
    bits = map (`getBit` pxs) bitCoords
    bitCoords = [Coord (x - 1) (y - 1), Coord x (y - 1), Coord (x + 1) (y - 1), Coord (x - 1) y, Coord x y, Coord (x + 1) y, Coord (x - 1) (y + 1), Coord x (y + 1), Coord (x + 1) (y + 1)]

enhance :: Map.Map Coord Char -> [Char] -> Map.Map Coord Char
enhance pxs iea = Map.fromList $ recur (minX + 1) (minY + 1)
  where
    recur x y
      | x >= maxX = recur (minX + 1) (y + 1)
      | y >= maxY = []
      | otherwise = enhancePx (Coord x y) dPxs iea : recur (x + 1) y
    (Coord minX minY, Coord maxX maxY) = limits dPxs
    dPxs = dilate $ dilate pxs

showPxs :: Map.Map Coord Char -> String
showPxs pxs = recur minX minY
  where
    recur x y
      | x > maxX = '\n' : recur minX (y + 1)
      | y > maxY = ""
      | otherwise = pxs Map.! Coord x y : recur (x + 1) y
    (Coord minX minY, Coord maxX maxY) = limits pxs

applyEnhance :: Int -> Map.Map Coord Char -> [Char] -> Map.Map Coord Char
applyEnhance i pxs iea = recur i pxs
  where
    recur 0 pxs = pxs
    recur n pxs = recur (n - 1) $ enhance pxs iea

d20p1 :: String -> IO ()
d20p1 input = do
  let (iea, pxs) = parse input
  let dpxs = dilate $ dilate' '.' pxs
  putStrLn $ showPxs dpxs
  let epxs = applyEnhance 2 dpxs iea
  putStrLn $ showPxs epxs
  let result = length . filter (== '#') $ map snd $ Map.toList epxs
  putStrLn (show result ++ " lit pixels after enhancing twice")

d20p2 :: String -> IO ()
d20p2 input = do
  let (iea, pxs) = parse input
  let dpxs = dilate $ dilate' '.' pxs
  putStrLn $ showPxs dpxs
  let epxs = applyEnhance 50 dpxs iea
  putStrLn $ showPxs epxs
  let result = length . filter (== '#') $ map snd $ Map.toList epxs
  putStrLn (show result ++ " lit pixels after 50 enhance rounds")
