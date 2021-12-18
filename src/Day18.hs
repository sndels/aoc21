module Day18 where

import Data.Char (isDigit)

data SFNum = Number Int | Pair (SFNum, SFNum)

data Result = Nop | Split | ExplodeLeft Int | ExplodeRight Int | Reduced deriving (Eq, Show)

instance Show SFNum where
  show (Number a) = show a
  show (Pair (l, r)) = "[" ++ show l ++ "," ++ show r ++ "]"

parse :: String -> [SFNum]
parse input = map (fst . parseSFNum) $ lines input
  where
    parseSFNum (',' : cs) = parseSFNum cs
    parseSFNum ('[' : cs) = (Pair (l, r), drop 1 rest1)
      where
        (r, rest1) = parseSFNum rest0
        (l, rest0) = parseSFNum cs
    parseSFNum (']' : cs) = error "']' should have been dropped"
    parseSFNum cs = (Number . read $ takeWhile isDigit cs, dropWhile isDigit cs)

addRight :: Int -> SFNum -> SFNum
addRight n (Number a) = Number (a + n)
addRight n (Pair (l, r)) = Pair (l, addRight n r)

addLeft :: Int -> SFNum -> SFNum
addLeft n (Number a) = Number (a + n)
addLeft n (Pair (l, r)) = Pair (addLeft n l, r)

handleRightExplode :: SFNum -> (SFNum, Result) -> (SFNum, (SFNum, Result))
handleRightExplode l (r, result) = case result of
  ExplodeLeft a -> (addRight a l, (r, Reduced))
  result -> (l, (r, result))

reduceExplode :: SFNum -> (SFNum, Result)
reduceExplode = recur 0
  where
    recur _ (Number a) = (Number a, Nop)
    recur 3 (Pair (Number a, Pair (Number b, Number c))) = (Pair (Number (a + b), Number 0), ExplodeRight c)
    recur 3 (Pair (Pair (Number a, Number b), r)) = (Pair (Number 0, addLeft b r), ExplodeLeft a)
    recur level (Pair (l, r)) = (Pair (nnl, nr), result1)
      where
        (nnl, (nr, result1)) = case result0 of
          Nop -> handleRightExplode nl $ recur (level + 1) r
          ExplodeRight a -> (nl, (addLeft a r, Reduced))
          ExplodeLeft a -> (nl, (r, ExplodeLeft a))
          _ -> (nl, (r, result0))
        (nl, result0) = recur (level + 1) l

reduceSplit :: SFNum -> (SFNum, Result)
reduceSplit = recur
  where
    recur (Number a)
      | a >= 10 = (Pair (Number (a `div` 2), Number (a `div` 2 + a `mod` 2)), Split)
      | otherwise = (Number a, Nop)
    recur (Pair (l, r)) = (Pair (nl, nr), result1)
      where
        (nr, result1) = case result0 of
          Nop -> recur r
          _ -> (r, result0)
        (nl, result0) = recur l

reduce :: SFNum -> SFNum
reduce p = if result1 == Nop then nnp else reduce nnp
  where
    (nnp, result1) = case result0 of
      Nop -> reduceSplit np
      _ -> (np, result0)
    (np, result0) = reduceExplode p

(+!) :: SFNum -> SFNum -> SFNum
(Number lhs) +! (Number rhs) = reduce $ Pair (Number lhs, Number rhs)
(Number lhs) +! (Pair rhs) = reduce $ Pair (Number lhs, Pair rhs)
(Pair lhs) +! (Number rhs) = reduce $ Pair (Pair lhs, Number rhs)
(Pair lhs) +! (Pair rhs) = reduce $ Pair (Pair lhs, Pair rhs)

magnitude :: SFNum -> Int
magnitude (Number a) = a
magnitude (Pair (l, r)) = 3 * magnitude l + 2 * magnitude r

d18p1 :: String -> IO ()
d18p1 input = do
  let pairs = parse input
  let reduced = foldl1 (+!) pairs
  let result = magnitude reduced
  putStrLn ("Magnitude of the reduced sum is " ++ show result)

largestSum :: [SFNum] -> Int
largestSum [] = 0
largestSum [p] = 0
largestSum (p : ps) = max s (largestSum ps)
  where
    s = maximum $ map (\pp -> max (magnitude (p +! pp)) (magnitude (pp +! p))) ps

d18p2 :: String -> IO ()
d18p2 input = do
  let pairs = parse input
  let result = largestSum pairs
  putStrLn ("Largest magnitude of any sum of two numbers is " ++ show result)
