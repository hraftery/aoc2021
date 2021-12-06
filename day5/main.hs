module Main where

import Data.Map  (Map)
import qualified Data.Map as M
import Data.Char (isDigit)
import Data.List (sort)

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

type Coord = (Int,Int)
type Count = Int


parseInput :: [String] -> [(Coord,Coord)]
parseInput = map $ inputTokensToCoordPair . words . map (\c -> if c==',' then ' ' else c)

inputTokensToCoordPair :: [String] -> (Coord,Coord)
inputTokensToCoordPair [x1,y1,sep,x2,y2] = ((read x1, read y1), (read x2, read y2))
inputTokensToCoordPair x = error $ "Unexpected input tokens" ++ show x

isHozOrVert :: (Coord,Coord) -> Bool
isHozOrVert ((x1,y1),(x2,y2)) = x1 == x2 || y1 == y2

{-
coordsFromTo :: (Coord,Coord) -> [Coord]
coordsFromTo ((x1,y1),(x2,y2)) = let [xMin,xMax] = sort [x1,x2]
                                     [yMin,yMax] = sort [y1,y2]
                                 in [(x,y) | x<-[xMin..xMax], y<-[yMin..yMax]]
-}

coordsFromTo :: (Coord,Coord) -> [Coord]
coordsFromTo (c1@(x1,y1), c2@(x2,y2)) = if c1 == c2 then [c1]
                                        else zip (ordsFromTo x1 x2) (ordsFromTo y1 y2)

ordsFromTo :: Int -> Int -> [Int]
ordsFromTo o1 o2  = case compare o1 o2 of
                      EQ -> repeat o1
                      LT -> [o1..o2]
                      GT -> [o1,pred o1..o2]

makeCoordMap :: [(Coord,Coord)] -> Map Coord Count
makeCoordMap = foldr addLine M.empty where
  addLine  l m = foldr addCoord m $ coordsFromTo l
  addCoord c m = M.insertWith (+) c 1 m


part1 :: [String] -> Int
part1 = M.size . M.filter (>=2) . makeCoordMap . filter isHozOrVert . parseInput

part2 :: [String] -> Int
part2 = M.size . M.filter (>=2) . makeCoordMap . parseInput
