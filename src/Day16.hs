module Day16 where

import Data.Dynamic ( toDyn, dynTypeRep, fromDyn, Dynamic )
import Data.Bits ( Bits((.|.), shiftL) )
import Data.Typeable ( Proxy(..), typeRep, TypeRep )

-- Do parsing with a string since that's simpler to rig up with data that is not byte aligned
-- TODO: How ergonomic is it to juggle raw bytes and a bit offset in Haskell?

data Packet = Packet {pVer::Int, pId::Int, pData::Dynamic } deriving(Show)

binaryToInt :: [Char] -> Int
binaryToInt "0000" = 0
binaryToInt "0001" = 1
binaryToInt "0010" = 2
binaryToInt "0011" = 3
binaryToInt "0100" = 4
binaryToInt "0101" = 5
binaryToInt "0110" = 6
binaryToInt "0111" = 7
binaryToInt "1000" = 8
binaryToInt "1001" = 9
binaryToInt "1010" = 10
binaryToInt "1011" = 11
binaryToInt "1100" = 12
binaryToInt "1101" = 13
binaryToInt "1110" = 14
binaryToInt "1111" = 15
binaryToInt b = error ("Invalid binary Int '" ++ b ++ "'")

readLiteralValue :: [Char] -> (Int, [Char])
readLiteralValue dat = recur 0 0 dat
  where recur i v dat = if is == "1" then recur (i+1) nv rest else (nv,rest)
          where nv = v `shiftL` 4 .|. binaryToInt vs
                (is, vs) = splitAt 1 ds
                (ds, rest) = splitAt 5 dat

literal :: Int -> Int -> [Char] -> (Packet, [Char])
literal ver id dat = (Packet ver id $ toDyn v, rest)
  where (v,rest) = readLiteralValue dat

readTightNum :: [Char] -> Int
readTightNum = recur 0
  where recur c "" = c
        recur c bits = recur nc rest
          where nc = c `shiftL` 4 .|. binaryToInt ncBits
                (ncBits, rest) = splitAt 4 bits

readSizedOps :: [Char] -> [Packet]
readSizedOps "" = []
readSizedOps dat = p : readSizedOps rest
  where (p, rest) = packet dat

sizedOp :: [Char] -> ([Packet], [Char])
sizedOp dat = (readSizedOps subOps, rest1)
  where (subOps, rest1) = splitAt size rest0
        size = readTightNum ("0" ++ sizeBits)
        (sizeBits, rest0) = splitAt 15 dat


readNumOps :: Int -> [Char] -> ([Packet], [Char])
readNumOps c dat = recur c [] dat
  where recur 0 ps dat = (ps,dat)
        recur c ps dat = recur (c-1) (ps ++ [p]) rest
          where (p, rest) = packet dat

numOp :: [Char] -> ([Packet], [Char])
numOp dat = readNumOps num rest
  where num = readTightNum ("0" ++ sizeBits)
        (sizeBits, rest) = splitAt 11 dat

operation :: Int -> Int -> [Char] -> (Packet, [Char])
operation ver id dat = (Packet ver id $ toDyn  ps, rest1)
  where (ps, rest1) = if i == "0" then sizedOp rest0 else numOp rest0
        (i, rest0) = splitAt 1 dat

packet :: [Char] -> (Packet, [Char])
packet verIdData =
  if id == 4
    then literal ver id dat
    else operation ver id dat
  where id = binaryToInt $ "0" ++ idS
        (idS, dat) = splitAt 3 idData
        ver = binaryToInt $ "0" ++ verS
        (verS, idData) = splitAt 3 verIdData

hexToBinary :: Char -> [Char]
hexToBinary '0' = "0000"
hexToBinary '1' = "0001"
hexToBinary '2' = "0010"
hexToBinary '3' = "0011"
hexToBinary '4' = "0100"
hexToBinary '5' = "0101"
hexToBinary '6' = "0110"
hexToBinary '7' = "0111"
hexToBinary '8' = "1000"
hexToBinary '9' = "1001"
hexToBinary 'A' = "1010"
hexToBinary 'B' = "1011"
hexToBinary 'C' = "1100"
hexToBinary 'D' = "1101"
hexToBinary 'E' = "1110"
hexToBinary 'F' = "1111"
hexToBinary _ = ""

parse :: [Char] -> (Packet , [Char])
parse = packet . foldl1 (++) . map hexToBinary

literalContent :: TypeRep
literalContent = typeRep (Proxy:: Proxy Int)
operationContent :: TypeRep
operationContent = typeRep (Proxy:: Proxy [Packet])

showPacket :: Packet -> String
showPacket p
  | rep == literalContent = show $ fromDyn d (0 :: Int)
  | rep == operationContent = show . map showPacket $ fromDyn d []
  | otherwise = error "Invalid dynamic packet type"
    where ver = pVer p
          i = pId p
          rep = dynTypeRep d
          d = pData p


sumVersions :: Packet -> Int
sumVersions p
  | rep == literalContent = v
  | rep == operationContent = v + (sum . map sumVersions $ fromDyn d [])
  | otherwise = error "Invalid dynamic packet type"
    where rep = dynTypeRep d
          v = pVer p
          d = pData p

d16p1 :: String -> IO ()
d16p1 input = do
  let (p, rest) = parse input
  let result = sumVersions p
  putStrLn ("Sum of all packet versions is " ++ show result)

evaluateOp :: Int -> [Packet] -> Int
evaluateOp pId ps
  | pId == 0 = sum evs
  | pId == 1 = product evs
  | pId == 2 = minimum evs
  | pId == 3 = maximum evs
  | pId == 5 = fromEnum(head evs > last evs)
  | pId == 6 = fromEnum(head evs < last evs)
  | pId == 7 = fromEnum(head evs == last evs)
  | otherwise = error $ "Unknown packet id " ++ show pId
  where evs = map evaluate ps

evaluate :: Packet -> Int
evaluate (Packet pVer pId pData)
  | rep == literalContent = fromDyn pData (0::Int)
  | rep == operationContent = evaluateOp pId $ fromDyn pData []
  | otherwise = error "Invalid dynamic packet type"
    where rep = dynTypeRep pData


d16p2 :: String -> IO ()
d16p2 input = do
  let (p, rest) = parse input
  let result = evaluate p
  putStrLn ("The expression evaluates to " ++ show result)
