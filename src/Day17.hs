module Day17 where

import Data.List
import Debug.Trace
import Text.Regex.TDFA ((=~))

parse :: String -> ((Int, Int), (Int, Int))
parse input = ((read minX, read maxX), (read minY, read maxY))
  where
    (_, _, _, [minX, maxX, minY, maxY]) = input =~ "target area: x=(.+)\\.\\.(.+), y=(.+)\\.\\.(.+)" :: (String, String, String, [String])

hits v (x, y) (minX, maxX) (minY, maxY) = recur v (x, y)
  where
    recur (vx, vy) (px, py)
      | px >= minX && px <= maxX && py >= minY && py <= maxY = True
      | vy < 0 && py < minY = False
      | otherwise = recur (max 0 (vx - 1), vy - 1) (px + vx, py + vy)

maxY vy = recur vy 0
  where
    recur 0 p = p
    recur v p = recur (v - 1) (p + v)

d17p1 :: String -> IO ()
d17p1 input = do
  let (rx, ry) = parse input
  let vyMax = max (abs $ fst ry) (abs $ snd ry)
  let vy = snd . last $ filter (\v -> hits v (fst rx, 0) rx ry) $ zip (repeat 0) [negate vyMax .. vyMax]
  let result = maxY vy
  putStrLn ("Highest y position for a hit trajectory is " ++ show result)

hitsX vx (minX, maxX) = recur vx 0
  where
    recur 0 p = hits p
    recur v p = hits p || recur (v - 1) (p + v)
    hits p = p >= minX && p <= maxX

d17p2 :: String -> IO ()
d17p2 input = do
  let (rx, ry) = parse input
  let vxMax = max (abs $ fst rx) (abs $ snd rx)
  let vyMax = max (abs $ fst ry) (abs $ snd ry)
  let vxs = filter (`hitsX` rx) [0 .. vxMax]
  let vys = map snd . filter (\v -> hits v (fst rx, 0) rx ry) $ zip (repeat 0) [negate vyMax .. vyMax]
  let allVs = foldl1 (++) $ map (zip vxs . repeat) vys
  let vs = filter (\v -> hits v (0, 0) rx ry) allVs
  let result = length vs
  putStrLn (show result ++ " distinct initial velocities that will hit")
