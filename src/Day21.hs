module Day21 where

import Data.Char (ord)
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map
import Debug.Trace

data Player = Player {n :: Int, score :: Int, pos :: Int} deriving (Show, Eq, Ord)

parse :: String -> (Player, Player)
parse input = (p1, p2)
  where
    p1 = Player 1 0 . (\c -> ord c - ord '0') . last $ head ls
    p2 = Player 2 0 . (\c -> ord c - ord '0') . last $ last ls
    ls = lines input

mod' :: Int -> Int -> Int
mod' n c = ((c - 1) `mod` n) + 1

roll :: Int -> ((Int, Int, Int), Int)
roll d = ((r1, r2, r3), nd)
  where
    nd = mod' 100 (r3 + 1)
    r3 = mod' 100 (r2 + 1)
    r2 = mod' 100 (r1 + 1)
    r1 = d

doRoll :: Int -> Int -> (Int, Int, Int) -> (Int, Int)
doRoll s p (r1, r2, r3) = (ns, np)
  where
    ns = s + np
    np = mod' 10 (r3 + mod' 10 (r2 + mod' 10 (r1 + p)))

play :: (Player, Player) -> (Int, Player, Player)
play = recur 1 0
  where
    recur d nr (Player n s p, other) =
      if ns >= 1000
        then ((nr + 1) * 3, Player n ns np, other)
        else recur nd (nr + 1) (other, Player n ns np)
      where
        (ns, np) = doRoll s p rolls
        (rolls, nd) = roll d

d21p1 :: String -> IO ()
d21p1 input = do
  let (p1, p2) = parse input
  let (nr, w, l) = play (p1, p2)
  putStrLn ("Player " ++ show (n w) ++ " won with " ++ show (score w) ++ " points.")
  let result = nr * score l
  putStrLn ("Losing score (" ++ show (score l) ++ ") times the number of dice rolls (" ++ show nr ++ ") was " ++ show result)

uniqueThrows :: [(Int, Int, Int)]
uniqueThrows = recur []
  where
    recur [a, b, c] = [(a, b, c)]
    recur rs = recur (1 : rs) ++ recur (2 : rs) ++ recur (3 : rs)

uniqueTurns :: [((Int, Int, Int), (Int, Int, Int))]
uniqueTurns = concatMap (zip uniqueThrows . repeat) uniqueThrows

doSingle :: Player -> (Int, Int, Int) -> Player
doSingle (Player i s p) (r1, r2, r3) = Player i ns np
  where
    ns = s + np
    np = mod' 10 (r3 + mod' 10 (r2 + mod' 10 (r1 + p)))

doTurn :: (Player, Player) -> ((Int, Int, Int), (Int, Int, Int)) -> (Player, Player)
doTurn (p1, p2) (p1rs, p2rs) = (doSingle p1 p1rs, doSingle p2 p2rs)

playMemo :: (Player, Player) -> (Int, Int)
playMemo state = snd $ recur Map.empty state
  where
    recur outcomes state
      | p1s >= 21 = (Map.insert state (1,0)outcomes , (1,0))
      | p2s >= 21 = (Map.insert state (0,1)outcomes , (0,1))
      | state `Map.member` outcomes = (outcomes, outcomes Map.! state)
      | otherwise = (Map.insert state ws newOutcomes, ws)
      where
        (newOutcomes, ws) = foldr (\s (ocs, ws) -> acc ws $ recur ocs s) (outcomes, (0,0)) newStates
        acc (p1ws, p2ws) (nOcs, (np1ws, np2ws)) = (nOcs, (p1ws + np1ws, p2ws + np2ws))
        newStates = map (doTurn state) uniqueTurns
        (Player _ p1s p1p) = p1
        (Player _ p2s p2p) = p2
        (p1, p2) = state

d21p2 :: String -> IO ()
d21p2 input = do
  let startState = parse input
  let (fw, sw) = playMemo startState
  putStrLn ("Player 1 wins " ++ show fw ++ " times and player 2 " ++ show sw ++ " times")
