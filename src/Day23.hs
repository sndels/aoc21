module Day23 where

import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.OrdPSQ as PQ
import qualified Data.Set as Set
import Debug.Trace

d23p1 :: p -> IO ()
d23p1 input = do
  putStrLn "Pen and paper says '14510'"

-- Either this implementation is really slow (which it likely is) or this would
-- greatly benefit from a heuristic distance function (which it would since D dominates cost)

type Room = [Char]

type Corridor = [Char]

data State = State {corridor :: Corridor, rooms :: [Room]} deriving (Eq, Ord, Show)

parse :: [Char] -> State
parse input = State emptyCorridor [[r00,'D','D',r01], [r10,'C','B',r11], [r20,'B', 'A',r21], [r30,'A','C',r31]]
  where
    [r00, r01] = map (!! 3) ls
    [r10, r11] = map (!! 5) ls
    [r20, r21] = map (!! 7) ls
    [r30, r31] = map (!! 9) ls
    ls = drop 2 . reverse . drop 1 . reverse $ lines input

energy :: Char -> Int -> Int
energy a d = stepCost a * d

type PQueue = PQ.OrdPSQ State Int Int

corridorEntrance :: Int -> Int
corridorEntrance ri = case ri of
  0 -> 2
  1 -> 4
  2 -> 6
  3 -> 8
  _ -> error "invalid room index"

targetRoom :: Char -> Int
targetRoom a = case a of
  'A' -> 0
  'B' -> 1
  'C' -> 2
  'D' -> 3
  _ -> -1

roomAmphi :: Int -> Char
roomAmphi i = case i of
  0 -> 'A'
  1 -> 'B'
  2 -> 'C'
  3 -> 'D'
  _ -> ' '

stepCost :: Char -> Int
stepCost a = case a of
  'A' -> 1
  'B' -> 10
  'C' -> 100
  'D' -> 1000
  _ -> error "no step cost"

emptyCorridor :: Corridor
emptyCorridor = [' ', ' ', 'X', ' ', 'X', ' ', 'X', ' ', 'X', ' ', ' ']

solveState :: State
solveState = State emptyCorridor [replicate 4 'A', replicate 4 'B', replicate 4 'C', replicate 4 'D']

isEmpty :: Char -> Bool
isEmpty c = c == ' ' || c == 'X'

roomClear :: Char -> Room -> Bool
roomClear c room = all (== ' ') $ filter (/= c) room

canReachRoom :: Int -> State -> Bool
canReachRoom ci state = notEmpty && roomClear c room && routeClear
  where
    routeClear = all isEmpty . take (i1 - i0) $ drop i0 corridor
    -- Drop 1 from min, since it can be the amphi itself and dropping the room entrance does not matter
    (i0, i1) = (min ci rci + 1, max ci rci)
    room = rooms !! ri
    rci = corridorEntrance ri
    ri = targetRoom c
    notEmpty = not (isEmpty c)
    c = corridor !! ci
    (State corridor rooms) = state

swapI :: Int -> a -> [a] -> [a]
swapI i a as = nas
  where
    nas = h ++ [a] ++ t
    (h, _ : t) = splitAt i as

putRoom :: Int -> State -> (Int, State)
putRoom ci (State corridor rooms) = (energy a (rDist + cDist), nState)
  where
    nState = State nc nrs
    nc = swapI ci ' ' corridor
    nrs = swapI ri nr rooms
    (rDist, nr) = put a 1 [] $ rooms !! ri
    put a n prev (' ' : ' ' : rs) = put a (n + 1) (prev ++ [' ']) (' ' : rs)
    put a n prev (' ' : rs) = (n, prev ++ a : rs)
    put _ _ _ _ = error "invalid putRoom put"
    cDist = abs (ci - rci)
    rci = corridorEntrance ri
    ri = targetRoom a
    a = corridor !! ci

moveIn :: Int -> State -> Maybe (Int, State)
moveIn ci state =
  if canReachRoom ci state
    then Just $ putRoom ci state
    else Nothing

popRoom :: Int -> [Room] -> Maybe (Char, Int, [Room])
popRoom i rooms =
  (\(a, n, rs) -> Just (a, n, rs)) =<< newRooms
  where
    newRooms = (\(a, n, nr) -> Just (a, n, swapI i nr rooms)) =<< newRoom
    newRoom =
      if roomClear (roomAmphi i) room
        then Nothing
        else Just (recur 1 [] room)
    recur _ prev [] = error "invalid popRoom recur"
    recur n prev (' ' : rs) = recur (n + 1) (prev ++ [' ']) rs
    recur n prev (r : rs) = (r, n, prev ++ [' '] ++ rs)
    room = rooms !! i

moveOuts :: Int -> State -> [(Int, State)]
moveOuts ri state = map (\(corr, cost) -> (cost, State corr newRooms)) newCorridors
  where
    newRooms = maybe rooms (\(_, _, rs) -> rs) popd
    newCorridors = maybe [] (\(a, rc, _) -> map (\(ci, cc) -> (swapI ci a corridor, energy a (rc + cc))) cMoves) popd
    cMoves = map (\i -> (i, abs (rci - i))) $ clearLeft ++ clearRight
    clearRight = map fst . filter ((/= 'X') . snd) . takeWhile (isEmpty . snd) $ drop rci cis
    clearLeft = map fst . filter ((/= 'X') . snd) . takeWhile (isEmpty . snd) . reverse $ take rci cis
    cis = zip [0 ..] corridor
    rci = corridorEntrance ri
    popd = popRoom ri rooms
    (State corridor rooms) = state

updateNeightbour :: (State, Int) -> (PQueue, Map.Map State Int) -> (PQueue, Map.Map State Int)
updateNeightbour (state, newEnergy) (pq, costs) =
  if newEnergy < oldEnergy
    then (snd $ PQ.alter alterPQ state pq, Map.insert state newEnergy costs)
    else (pq, costs)
  where
    alterPQ _ = (0, Just (newEnergy, 0))
    oldEnergy = Map.findWithDefault maxBound state costs

neighbours :: State -> Set.Set State -> [(Int, State)]
neighbours state visited = filter (flip notElem visited . snd) $ pus ++ mos
  where
    pus = mapMaybe (uncurry moveIn) . zip [0 .. 10] $ repeat state
    mos = foldr1 (++) $ map (`moveOuts` state) [0 .. 3]

dijkstra :: State -> Int
dijkstra start = recur Set.empty startCosts startPQ
  where
    startCosts = Map.insert start 0 Map.empty
    startPQ = PQ.insert start 0 0 PQ.empty
    recur visited costs pq
      | state == solveState = costs Map.! solveState
      | otherwise = recur newVisited ncosts nnpq
      where
        (nnpq, ncosts) = foldr updateNeightbour (npq, costs) nes
        nes = map (\(e, state) -> (state, currentEnergy + e)) ns
        ns = neighbours state newVisited
        newVisited = Set.insert state visited
        (state, currentEnergy, _,npq) = fromJust $ PQ.minView pq

d23p2 :: String -> IO ()
d23p2 input = do
  let start = parse input
  let energy = dijkstra start
  putStrLn ("At least " ++ show energy ++ " is required")
