module Day22 where

import Data.Foldable (Foldable (foldl'))
import Data.Maybe (isJust, mapMaybe)
import Text.Regex.TDFA ((=~))

data Coord = Coord {coordX :: Int, coordY :: Int, coordZ :: Int} deriving (Eq, Ord)

instance Show Coord where
  show (Coord x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

tCoord :: Coord -> (Int, Int, Int)
tCoord (Coord x y z) = (x, y, z)

mini :: Coord -> Coord -> Coord
mini (Coord x0 y0 z0) (Coord x1 y1 z1) = Coord (min x0 x1) (min y0 y1) (min z0 z1)

maxi :: Coord -> Coord -> Coord
maxi (Coord x0 y0 z0) (Coord x1 y1 z1) = Coord (max x0 x1) (max y0 y1) (max z0 z1)

(<=!) :: Coord -> Coord -> Bool
(Coord x0 y0 z0) <=! (Coord x1 y1 z1) = x0 <= x1 && y0 <= y1 && z0 <= z1

data Cuboid = Cuboid {cuboidMin :: Coord, cuboidMax :: Coord}

instance Show Cuboid where
  show (Cuboid c0 c1) = show c0 ++ "->" ++ show c1

cuboidCubes :: Cuboid -> Int
cuboidCubes (Cuboid (Coord minX minY minZ) (Coord maxX maxY maxZ)) =
  (maxX - minX + 1) * (maxY - minY + 1) * (maxZ - minZ + 1)

contains :: Cuboid -> Cuboid -> Bool
contains (Cuboid c0Min c0Max) (Cuboid c1Min c1Max) = c0Min <=! c1Min && c1Max <=! c0Max

cuboidIntersection :: Cuboid -> Cuboid -> Maybe Cuboid
cuboidIntersection c0 c1 = if minC <=! maxC then Just (Cuboid minC maxC) else Nothing
  where
    maxC = mini c0Max c1Max
    minC = maxi c0Min c1Min
    (Cuboid c0Min c0Max) = c0
    (Cuboid c1Min c1Max) = c1

intersects :: Cuboid -> Cuboid -> Bool
intersects c0 c1 = isJust $ cuboidIntersection c0 c1

cuboidDifference :: Cuboid -> Cuboid -> [Cuboid]
cuboidDifference c0 c1 =
  if intersects c0 c1
    then
      if contains c1 c0
        then []
        else recur c0 c1
    else [c0]
  where
    recur (Cuboid c0Min c0Max) c1
      | c0MinX < c1MinX =
        Cuboid c0Min (Coord (c1MinX - 1) c0MaxY c0MaxZ) :
        recur (Cuboid (Coord c1MinX c0MinY c0MinZ) c0Max) c1
      | c0MinY < c1MinY =
        Cuboid c0Min (Coord c0MaxX (c1MinY - 1) c0MaxZ) :
        recur (Cuboid (Coord c0MinX c1MinY c0MinZ) c0Max) c1
      | c0MinZ < c1MinZ =
        Cuboid c0Min (Coord c0MaxX c0MaxY (c1MinZ - 1)) :
        recur (Cuboid (Coord c0MinX c0MinY c1MinZ) c0Max) c1
      | c0MaxX > c1MaxX =
        Cuboid (Coord (c1MaxX + 1) c0MinY c0MinZ) c0Max :
        recur (Cuboid c0Min (Coord c1MaxX c0MaxY c0MaxZ)) c1
      | c0MaxY > c1MaxY =
        Cuboid (Coord c0MinX (c1MaxY + 1) c0MinZ) c0Max :
        recur (Cuboid c0Min (Coord c0MaxX c1MaxY c0MaxZ)) c1
      | c0MaxZ > c1MaxZ =
        Cuboid (Coord c0MinX c0MinY (c1MaxZ + 1)) c0Max :
        recur (Cuboid c0Min (Coord c0MaxX c0MaxY c1MaxZ)) c1
      | otherwise = []
      where
        (Coord c1MaxX c1MaxY c1MaxZ) = c1Max
        (Coord c1MinX c1MinY c1MinZ) = c1Min
        (Coord c0MaxX c0MaxY c0MaxZ) = c0Max
        (Coord c0MinX c0MinY c0MinZ) = c0Min
        (Cuboid c1Min c1Max) = c1

data Step = Step {stepOp :: String, stepCuboid :: Cuboid} deriving (Show)

parseStep :: String -> Step
parseStep l = Step op (Cuboid (Coord x0 y0 z0) (Coord x1 y1 z1))
  where
    [x0, x1, y0, y1, z0, z1] = map read coords
    (op : coords) = targets
    (_, _, _, targets) = l =~ "(.+) x=(.+)\\.\\.(.+),y=(.+)\\.\\.(.+),z=(.+)\\.\\.(.+)" :: (String, String, String, [String])

parse :: String -> [Step]
parse input = map parseStep $ lines input

-- This could simplify the result cuboids, but that might actually be slower since we don't need the smallest set
doStep :: [Cuboid] -> Step -> [Cuboid]
doStep cuboids (Step op cuboid)
  | null cuboids = [cuboid | op == "on"]
  | op == "on" =
    cuboids ++ foldl' (\cs c -> concatMap (`cuboidDifference` c) cs) [cuboid] cuboids
  | otherwise = concatMap (`cuboidDifference` cuboid) cuboids

countCubes :: [Cuboid] -> Int
countCubes cuboids = sum $ map cuboidCubes cuboids

clipRef :: [Cuboid] -> [Cuboid]
clipRef = mapMaybe (`cuboidIntersection` ref)
  where
    ref = Cuboid (Coord (negate 50) (negate 50) (negate 50)) (Coord 50 50 50)

d22p1 :: String -> IO ()
d22p1 input = do
  let steps = parse input
  let cuboids = foldl' (\cuboids s -> clipRef $ doStep cuboids s) [] steps
  let cubes = countCubes cuboids
  putStrLn (show cubes ++ " cubes are on in the region")

d22p2 :: String -> IO ()
d22p2 input = do
  let steps = parse input
  let cuboids = foldl' doStep [] steps
  let cubes = countCubes cuboids
  putStrLn (show cubes ++ " cubes are on")
