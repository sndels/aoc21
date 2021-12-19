module Day19 where

import Data.Foldable
import Data.List (deleteBy, find, findIndex, intersect)
import Data.Maybe (fromJust)
import Debug.Trace
import Text.Regex.TDFA ((=~))

data Coord = Coord Int Int Int deriving (Eq)

instance Show Coord where
  show (Coord x y z) = "(" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ")"

instance Num Coord where
  fromInteger d = Coord (fromInteger d) (fromInteger d) (fromInteger d)
  signum (Coord x y z) = Coord (signum x) (signum y) (signum z)
  abs (Coord x y z) = Coord (abs x) (abs y) (abs z)
  negate (Coord x y z) = Coord (negate x) (negate y) (negate z)
  Coord x0 y0 z0 + Coord x1 y1 z1 = Coord (x0 + x1) (y0 + y1) (z0 + z1)
  b0 * b1 = error "multiply is meaningless for Coords"

rotX :: Coord -> Coord
rotX (Coord x y z) = Coord x (negate z) y

rotY :: Coord -> Coord
rotY (Coord x y z) = Coord (negate z) y x

rotZ :: Coord -> Coord
rotZ (Coord x y z) = Coord y (negate x) z

apply 1 op = op
apply n op = op . apply (n - 1) op

tryParseCoord :: String -> Maybe Coord
tryParseCoord l =
  if length target == 3
    then Just $ Coord (read (head target)) (read (target !! 1)) (read (target !! 2))
    else Nothing
  where
    (_, _, _, target) = l =~ "(.+),(.+),(.+)" :: (String, String, String, [String])

tryParseHeader :: String -> Maybe Int
tryParseHeader l =
  if length target == 1
    then Just . read $ head target
    else Nothing
  where
    (_, _, _, target) = l =~ "--- scanner (.+) ---" :: (String, String, String, [String])

data Scanner = Scanner Int [Coord]

instance Show Scanner where
  show (Scanner n bs) = foldl' (\acc b -> acc ++ "\n" ++ show b) header bs
    where
      header = "--- scanner " ++ show n ++ "---"

parse :: String -> [Scanner]
parse = recur 0 [] . lines
  where
    recur n bs [] = [Scanner n bs]
    recur n bs (l : ls) =
      case tryParseHeader l of
        Just n -> recur n [] ls
        Nothing ->
          case tryParseCoord l of
            Just b -> recur n (bs ++ [b]) ls
            Nothing -> Scanner n bs : recur 0 [] ls

delta :: Coord -> [Coord] -> [Coord]
delta ob = map (\b -> b - ob)

-- Returns beacon distances relative to all beacons
deltas :: [Coord] -> [(Coord, [Coord])]
deltas bs = map (\b -> (b, delta b bs)) bs

amendDelta :: [Coord] -> (Coord, [Coord]) -> (Coord, [Coord])
amendDelta nbs (c, bs) = (c, map (\b -> b - c) nbs ++ bs)

addToDeltas :: [(Coord, [Coord])] -> [Coord] -> [Coord] -> [(Coord, [Coord])]
addToDeltas ds bs newBs = newDs ++ amendedDs
  where
    newDs = take (length newBs) $ deltas (newBs ++ bs)
    amendedDs = map (amendDelta newBs) ds

rotPermutations :: [Coord -> Coord]
rotPermutations = [id, rotX, apply 2 rotX, apply 3 rotX, rotY, apply 2 rotY, apply 3 rotY, rotZ, apply 2 rotZ, apply 3 rotZ, rotZ . rotY, apply 2 rotZ . rotY, apply 3 rotZ . rotY, rotX . apply 2 rotY, apply 2 rotX . apply 2 rotY, apply 3 rotX . apply 2 rotY, rotZ . apply 3 rotY, apply 2 rotZ . apply 3 rotY, apply 3 rotZ . apply 3 rotY, rotY . rotZ, apply 2 rotY . rotZ, apply 3 rotY . rotZ, rotY . apply 3 rotZ, apply 2 rotY . apply 3 rotZ, apply 3 rotY . apply 3 rotZ]

-- Returns rotation permutations relative to all beacons
permutations :: Scanner -> (Scanner, [[(Coord, [Coord])]])
permutations (Scanner n bs) = (Scanner n bs, perms)
  where
    perms = map (deltas . (`map` bs)) rotPermutations

matches :: [Coord] -> (Coord, [Coord]) -> Bool
matches bs0 (_, bs1) = Coord 0 0 0 `elem` matching && 12 <= length matching
  where
    matching = bs0 `intersect` bs1

newtype DeltaMatch = DeltaMatch {dBeacon :: Int}

matchDelta :: [Coord] -> [(Coord, [Coord])] -> Maybe DeltaMatch
matchDelta bs = recur 0
  where
    recur _ [] = Nothing
    recur i (d : ds) =
      if matches bs d
        then Just (DeltaMatch i)
        else recur (i + 1) ds

data RotationMatch = RotationMatch {rRotation :: Int, rBeacon :: Int}

matchRotation :: [Coord] -> [[(Coord, [Coord])]] -> Maybe RotationMatch
matchRotation bs = recur 0
  where
    recur _ [] = Nothing
    recur i (r : rs) = case matchDelta bs r of
      Just (DeltaMatch db) -> Just (RotationMatch i db)
      Nothing -> recur (i + 1) rs

data ScannerMatch = ScannerMatch {sNum :: Int, sRotation :: Int, sBeacon :: Int}

matchScanner :: [Coord] -> [(Scanner, [[(Coord, [Coord])]])] -> Maybe ScannerMatch
matchScanner bs = recur
  where
    recur [] = Nothing
    recur ((Scanner n _, rots) : ss) = case matchRotation bs rots of
      Just (RotationMatch rr rb) -> Just (ScannerMatch n rr rb)
      Nothing -> recur ss

data Match = Match {selfBeacon :: Int, otherId :: Int, otherRotation :: Int, otherBeacon :: Int}

matchDeltaScanner :: [(Coord, [Coord])] -> [(Scanner, [[(Coord, [Coord])]])] -> Match
matchDeltaScanner perms scanners = recur 0 perms
  where
    recur i [] = error "no match found"
    recur i ((_, p) : ps) = case matchScanner p scanners of
      Just (ScannerMatch sn sr sb) -> Match i sn sr sb
      Nothing -> recur (i + 1) ps

data Space = Space {scanners :: [(Int, Coord)], beacons :: [Coord]}

reduce :: [Scanner] -> Space
reduce (Scanner n bs : ss) = recur [(n, Coord 0 0 0)] bs (deltas bs) $ map permutations ss
  where
    recur scanners bs _ [] = Space scanners bs
    recur scanners bs bPerms ss = trace (show (length ss) ++ " scanners left") recur (newScanner : scanners) (bs ++ newBeacons) newPerms ssWithoutOther
      where
        newPerms = addToDeltas bPerms bs newBeacons
        newScanner = (otherNum, selfCenterPos - otherCenterPos)
        newBeacons = map (selfCenterPos +) prunedRotatedBs

        prunedRotatedBs = filter (`notElem` commonCentered) otherCenteredBeacons

        commonCentered = selfCenteredBeacons `intersect` otherCenteredBeacons

        otherCenteredBeacons = delta otherCenterPos otherRotatedBeacons
        otherCenterPos = otherRotatedBeacons !! otherBeacon

        otherRotatedBeacons = map rotation otherBeacons
        rotation = rotPermutations !! otherRotation

        (Scanner _ otherBeacons, _) = fromJust $ find (\(Scanner n _, _) -> n == otherNum) ss
        ssWithoutOther = filter (\(Scanner n _, _) -> n /= otherNum) ss

        (selfCenterPos, selfCenteredBeacons) = bPerms !! selfBeacon

        (Match selfBeacon otherNum otherRotation otherBeacon) = matchDeltaScanner bPerms ss
reduce _ = error "invalid reduce"

d19p1 :: String -> IO ()
d19p1 input = do
  let scanners = parse input
  let (Space ss bs) = reduce scanners
  let result = length bs
  putStrLn ("Full map has " ++ show result ++ " beacons")

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance c0 c1 = x + y + z
  where
    (Coord x y z) = abs (c0 - c1)

d19p2 :: String -> IO ()
d19p2 input = do
  let scanners = parse input
  let (Space ss bs) = reduce scanners
  let ((a, b), dist) = maximumBy (\a b -> compare (snd a) (snd b)) . foldl1 (++) $ map (\(a, c0) -> map (\(b, c1) -> ((a, b), manhattanDistance c0 c1)) ss) ss
  putStrLn ("Maximum manhattan distance of " ++ show dist ++ " is between scanners " ++ show a ++ " and " ++ show b)
