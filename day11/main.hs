module Main where

import Data.Char (digitToInt)
import Data.List.Split (chunksOf)

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)


type EnergyMap = [[Int]]
type Coord = (Int,Int)

energy :: EnergyMap -> Coord -> Int
energy m (c,r) = m !! r !! c

size :: EnergyMap -> (Int,Int)
size m = (length m, length $ head m)

allCoords :: EnergyMap -> [[Coord]]
allCoords m = let (nRows,nCols) = size m
              in chunksOf nCols [(c-1,r-1) | r <- [1..nRows], c <- [1..nCols]]


parseInput :: [String] -> EnergyMap
parseInput = map (map digitToInt)

neighbours :: EnergyMap -> Coord -> [Coord]
neighbours m (c,r)  = filter (isValidCoord m) [(c', r')  | r' <- [r-1..r+1], c' <- [c-1..c+1],
                                                           not (c' == c && r' == r)]

isValidCoord :: EnergyMap -> Coord -> Bool
isValidCoord m (c,r)  = let (nRows,nCols) = size m in
                        c >= 0 && r >= 0 && c < nCols && r < nRows


step :: EnergyMap -> EnergyMap
step = applyFlashes . map (map (+1))

stepAndCountFlashes :: (Int, EnergyMap) -> (Int, EnergyMap)
stepAndCountFlashes (n, m)  = let m' = step m
                                  n' = length . filter (==0) $ concat m in
                              (n+n', m')

steps :: Int -> EnergyMap -> EnergyMap
steps n m = iterate step m !! n

stepsAndCountFlashes :: Int -> EnergyMap -> (Int, EnergyMap)
stepsAndCountFlashes n m = iterate stepAndCountFlashes (0, m) !! n


applyFlashes :: EnergyMap -> EnergyMap
applyFlashes m  = let coords = allCoords m in
                  if any (any (isFlasher m)) coords
                  then applyFlashes $ map (map f) coords
                  else m
  where f c = case energy m c of
                10 -> 0
                0  -> 0
                e  -> minimum [10, e + numAdjacentFlashers m c]

isFlasher :: EnergyMap -> Coord -> Bool
isFlasher m c = (>9) $ energy m c

numAdjacentFlashers :: EnergyMap -> Coord -> Int
numAdjacentFlashers m = length . filter (isFlasher m) . neighbours m

part1 :: [String] -> Int
part1 = fst . stepsAndCountFlashes 101 . parseInput

part2 :: [String] -> Int
part2 = f 0 . parseInput where
  f n m
    | all (all (==0)) m  = n
    | otherwise          = f (succ n) (step m)
