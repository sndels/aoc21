module Day24 where

import Data.Dynamic (Dynamic, dynTypeRep, fromDyn, toDyn)
import Data.Foldable (Foldable (foldl'))
import Data.List (foldl', groupBy, sortBy)
import Data.List.Split (chunksOf, splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isJust)
import Data.Typeable (Proxy (..), TypeRep, typeRep)
import Text.Read (readMaybe)

-- See Day14Decode.txt for the investigation, all of the code is still here

data ALU = ALU {x :: Int, y :: Int, z :: Int, w :: Int} deriving (Show)

initALU :: ALU
initALU = ALU 0 0 0 0

store :: Char -> Int -> ALU -> ALU
store reg v (ALU x y z w)
  | reg == 'x' = ALU v y z w
  | reg == 'y' = ALU x v z w
  | reg == 'z' = ALU x y v w
  | reg == 'w' = ALU x y z v
  | otherwise = error ("invalid store register" ++ show reg)

load :: Char -> ALU -> Int
load reg (ALU x y z w)
  | reg == 'x' = x
  | reg == 'y' = y
  | reg == 'z' = z
  | reg == 'w' = w
  | otherwise = error ("invalid load register" ++ show reg)

data DALU = DALU {dx :: String, dy :: String, dz :: String, dw :: String} deriving (Show)

initDALU :: DALU
initDALU = DALU "0" "0" "0" "0"

storeD :: Char -> String -> DALU -> DALU
storeD reg v (DALU x y z w)
  | reg == 'x' = DALU v y z w
  | reg == 'y' = DALU x v z w
  | reg == 'z' = DALU x y v w
  | reg == 'w' = DALU x y z v
  | otherwise = error ("invalid storeD register" ++ show reg)

loadD :: Char -> DALU -> String
loadD reg (DALU x y z w)
  | reg == 'x' = x
  | reg == 'y' = y
  | reg == 'z' = z
  | reg == 'w' = w
  | otherwise = error ("invalid loadD register" ++ show reg)

addOp :: Char -> Int -> ALU -> ALU
addOp a b alu = store a (av + b) alu
  where
    av = load a alu

mulOp :: Char -> Int -> ALU -> ALU
mulOp a b alu = store a (av * b) alu
  where
    av = load a alu

divOp :: Char -> Int -> ALU -> ALU
divOp a b alu = store a (av `div` b) alu
  where
    av = load a alu

modOp :: Char -> Int -> ALU -> ALU
modOp a b alu = store a (av `mod` b) alu
  where
    av = load a alu

eqlOp :: Char -> Int -> ALU -> ALU
eqlOp a b alu = store a (fromEnum $ av == b) alu
  where
    av = load a alu

data Op = Op {name :: String, a :: Char, b :: Dynamic} deriving (Show)

subprog :: (Int, Int, Int) -> Int -> Int -> Int
subprog (n0, n1, n2) z w = (z `div` n0 * ((25 * x) + 1)) + ((w + n2) * x)
  where
    x = fromEnum $ (z `mod` 26 + n1) /= w

charType :: TypeRep
charType = typeRep (Proxy :: Proxy Char)

intType :: TypeRep
intType = typeRep (Proxy :: Proxy Int)

parseOp :: [Char] -> Op
parseOp input = Op name a (maybe (toDyn $ head b) toDyn bc)
  where
    bc = readMaybe b :: Maybe Int
    b = if length parts == 3 then parts !! 2 else " "
    a = head $ parts !! 1
    name = head parts
    parts = splitOn " " input

parse :: String -> [Op]
parse input = map parseOp $ lines input

parseSubprog :: [[Char]] -> (Int, Int, Int)
parseSubprog ls = (n0, n1, n2)
  where
    n0 = read . last . splitOn " " $ ls !! 4
    n1 = read . last . splitOn " " $ ls !! 5
    n2 = read . last . splitOn " " $ ls !! 15

parseSubprogs :: String -> [(Int, Int, Int)]
parseSubprogs input = map parseSubprog $ chunksOf 18 $ lines input

run :: Op -> (ALU, [Int]) -> (ALU, [Int])
run (Op name a b) (alu, input)
  | name == "inp" = (store a (head input) alu, tail input)
  | name == "add" = (addOp a bv alu, input)
  | name == "mul" = (mulOp a bv alu, input)
  | name == "div" = (divOp a bv alu, input)
  | name == "mod" = (modOp a bv alu, input)
  | name == "eql" = (eqlOp a bv alu, input)
  | otherwise = error ("invalid op " ++ show name)
  where
    bv
      | bType == charType = load (fromDyn b 'x') alu
      | bType == intType = fromDyn b 0
      | otherwise = error ("invalid bType " ++ show bType)
    bType = dynTypeRep b

debugRun :: Op -> (DALU, [String]) -> (DALU, [String])
debugRun (Op name a b) (dalu, is)
  | name == "inp" = (storeD a (head is) dalu, tail is)
  | name == "add" =
    if bv == "0"
      then (dalu, is)
      else
        if av == "0"
          then (storeD a bv dalu, is)
          else
            if isJust ac && isJust bc
              then (storeD a (show (fromJust ac + fromJust bc)) dalu, is)
              else (storeD a ("(" ++ av ++ "+" ++ bv ++ ")") dalu, is)
  | name == "sub" =
    if bv == "0"
      then (dalu, is)
      else
        if av == "0"
          then
            if isJust bc
              then (storeD a (show $ negate $ fromJust bc) dalu, is)
              else (storeD a ("-" ++ bv) dalu, is)
          else
            if isJust ac && isJust bc
              then (storeD a (show (fromJust ac - fromJust bc)) dalu, is)
              else (storeD a ("(" ++ av ++ "-" ++ bv ++ ")") dalu, is)
  | name == "mul" =
    if av == "0" || bv == "0"
      then (storeD a "0" dalu, is)
      else
        if isJust ac && isJust bc
          then (storeD a (show (fromJust ac * fromJust bc)) dalu, is)
          else (storeD a ("(" ++ av ++ "*" ++ bv ++ ")") dalu, is)
  | name == "div" =
    if av == "0"
      then (storeD a "0" dalu, is)
      else
        if isJust ac && isJust bc
          then (storeD a (show (fromJust ac `div` fromJust bc)) dalu, is)
          else (storeD a ("(" ++ av ++ "/" ++ bv ++ ")") dalu, is)
  | name == "mod" =
    if av == "0"
      then (storeD a "0" dalu, is)
      else
        if isJust ac && isJust bc
          then (storeD a (show (fromJust ac `mod` fromJust bc)) dalu, is)
          else (storeD a ("(" ++ av ++ "`mod`" ++ bv ++ ")") dalu, is)
  | name == "eql" =
    if isJust ac && isJust bc
      then (storeD a (show $ fromEnum $ fromJust ac == fromJust bc) dalu, is)
      else (storeD a ("(" ++ av ++ "==" ++ bv ++ ")") dalu, is)
  | otherwise = error ("invalid op " ++ show name)
  where
    ac = readMaybe av :: Maybe Int
    bc = readMaybe bv :: Maybe Int
    bv
      | bType == charType = loadD (fromDyn b ' ') dalu
      | bType == intType = show $ fromDyn b (0 :: Int)
      | otherwise = error ("invalid bType " ++ show bType)
    bType = dynTypeRep b
    av = loadD a dalu

evaluateMONAD :: Foldable t => t Op -> [Int] -> (ALU, [Int])
evaluateMONAD monad number =
  foldl' (flip run) (initALU, number) monad

dumpMONAD :: Foldable t => t Op -> DALU
dumpMONAD monad =
  fst $ foldl' (flip debugRun) initState monad
  where
    initState = (DALU "0" "0" "0" "0", map (\i -> "i" ++ show i) [0 .. 13])

sub1 num = snd $ recur num
  where
    recur [i] =
      if i == 1
        then (True, [9])
        else (False, [i - 1])
    recur (i : is) =
      if carry
        then
          if i == 1
            then (True, 9 : rest)
            else (False, i - 1 : rest)
        else (False, i : rest)
      where
        (carry, rest) = recur is
    recur [] = error "invalid sub1"

findPassing progs criteria = recur 0 progs $ Map.fromList [(0, 0)]
  where
    recur 14 _ states = states Map.! 0
    recur i (p : ps) states = recur (i + 1) ps $ Map.fromList maxStates
      where
        maxStates = map criteria . groupBy (\(z0, _) (z1, _) -> z0 == z1) $ sortBy (\(z0, _) (z1, _) -> compare z0 z1) newStates
        newStates = concatMap (\(z, num) -> map (\n -> (subprog p z n, num * 10 + n)) $ filter (canPass z) nums) $ Map.toList states
        canPass z i = n0 /= 26 || (z `mod` 26 + n1) == i
        nums = [1 .. 9]
        (n0, n1, _) = p

d24p1 :: String -> IO ()
d24p1 input = do
  let ops = parse input
  let progs = parseSubprogs input
  let n = findPassing progs maximum
  putStrLn ("Largest accepted model number is " ++ show n)

d24p2 :: String -> IO ()
d24p2 input = do
  let ops = parse input
  let progs = parseSubprogs input
  let n = findPassing progs minimum
  putStrLn ("Smallest accepted model number is " ++ show n)
