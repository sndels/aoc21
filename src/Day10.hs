module Day10 where

import Data.Maybe
import Data.Foldable
import Data.List
import qualified Data.Map.Strict as Map

parse :: String -> [[Char]]
parse = lines

firstInvalidChar :: [Char]-> Maybe Char
firstInvalidChar = recur []
  where recur _ [] = Nothing
        recur [] (c:cs) = recur [c] cs
        recur (p:ps) (c:cs)
          | c `elem` ['(', '[', '{', '<'] = recur (c:p:ps) cs
          | p == '(' && c /= ')' = Just c
          | p == '[' && c /= ']' = Just c
          | p == '{' && c /= '}' = Just c
          | p == '<' && c /= '>' = Just c
          | otherwise = recur ps cs

d10p1 :: String -> IO ()
d10p1 input = do
  let ls = lines input
  let illegalChars =  mapMaybe firstInvalidChar ls
  let illegalPoints = Map.fromList [(')',3), (']', 57), ('}',1197), ('>',25137)]
  let score = sum $ map (illegalPoints Map.!) illegalChars
  putStrLn ("Total syntax error score is " ++ show score)

danglingChunks :: [Char] -> [Char]
danglingChunks = recur []
  where recur ps [] = ps
        recur [] (c:cs) = recur [c] cs
        recur (p:ps) (c:cs)
          | c `elem` ['(', '[', '{', '<'] = recur (c:p:ps) cs
          | p == '(' && c /= ')' = []
          | p == '[' && c /= ']' = []
          | p == '{' && c /= '}' = []
          | p == '<' && c /= '>' = []
          | otherwise = recur ps cs

completeChunks :: [Char] -> Int
completeChunks = foldl' (\s p -> s * 5 + points Map.! p) 0
  where points = Map.fromList [('(',1),('[', 2), ('{',3), ('<',4)]

d10p2 :: String -> IO ()
d10p2 input = do
  let ls = lines input
  let incompleteLs = filter (not . null) $ map danglingChunks ls
  let scores =sort $ map completeChunks incompleteLs
  let middle = scores !! (length scores `div` 2)
  putStrLn ("Middle autocomplete score is " ++ show middle)
