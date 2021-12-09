module Main where

import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

type HeightMap = [[Int]]
type Coord = (Int,Int)

height :: HeightMap -> Coord -> Int
height m (c,r) = m !! r !! c

size :: HeightMap -> (Int,Int)
size m = (length m, length $ head m)

parseInput :: [String] -> HeightMap
parseInput = map (map digitToInt)


lowPointHeight :: HeightMap -> Coord -> Maybe Int
lowPointHeight m c  = if isLowPoint m c then Just (height m c) else Nothing

isLowPoint :: HeightMap -> Coord -> Bool
isLowPoint m c  = all (\c' -> height m c' > height m c) (neighbours m c)

neighbours :: HeightMap -> Coord -> [Coord]
neighbours m (c,r)  = filter (isValidCoord m) [(c-1, r), (c+1, r), (c, r-1), (c,r+1)]

isValidCoord :: HeightMap -> Coord -> Bool
isValidCoord m (c,r)  = let (nRows,nCols) = size m in
                        c >= 0 && r >= 0 && c < nCols && r < nRows


mapHeightMap :: (HeightMap -> Coord -> Maybe Int) -> HeightMap -> [Int]
mapHeightMap f m  = let (nRows,nCols) = size m
                        coords        = [(c-1,r-1) | r <- [1..nRows], c <- [1..nCols]]
                    in mapMaybe (f m) coords 


lowPointSize :: HeightMap -> Coord -> Maybe Int
lowPointSize m c  = if isLowPoint m c then Just (S.size $ basin m c) else Nothing

basin :: HeightMap -> Coord -> Set Coord
basin = expandBasin S.empty where
  expandBasin s m c = if height m c == 9 || c `S.member` s then s
                      else let s' = S.insert c s in
                           foldl S.union s' [expandBasin s' m x | x <- neighbours m c,
                                                                  height m c < height m x]

part1 :: [String] -> Int
part1 = sum . map (+1) . mapHeightMap lowPointHeight . parseInput

part2 :: [String] -> Int
part2 = product . take 3 . reverse . sort . mapHeightMap lowPointSize . parseInput
