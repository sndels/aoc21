module Day21 where

import Data.Char ( ord )

data Player = Player {n :: Int, score :: Int, pos :: Int} deriving (Show)

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

play :: (Player, Player) -> (Int, Player, Player)
play = recur 1 0
  where
    recur d nr (Player n s p, other) =
      if ns >= 1000
        then ((nr + 1) * 3, Player n ns np, other)
        else recur nd (nr + 1) (other, Player n ns np)
      where
        ns = s + np
        np = mod' 10 (r3 + mod' 10 (r2 + mod' 10 (r1 + p)))
        ((r1, r2, r3), nd) = roll d

d21p1 :: String -> IO ()
d21p1 input = do
  let (p1, p2) = parse input
  let (nr, w, l) = play (p1, p2)
  putStrLn ("Player " ++ show (n w) ++ " won with " ++ show (score w) ++ " points.")
  let result = nr * score l
  putStrLn ("Losing score (" ++ show (score l) ++ ") times the number of dice rolls (" ++ show nr ++ ") was " ++ show result)

d21p2 :: String -> IO ()
d21p2 input = do
  putStr input
  putStrLn "Let's do this"
