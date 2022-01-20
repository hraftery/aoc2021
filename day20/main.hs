module Main where

import Data.List.Split (chunksOf)
import Data.Bifunctor (second)
import Data.Map (Map, (!))
import qualified Data.Map as M


main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)


type ImageEnhancementAlgorithm = Map Int Bool
type Coord = (Int,Int)
type Image = [[Bool]] -- true == '#' == "light pixel"

toPixel :: Char -> Bool
toPixel = (==) '#'

fromPixels :: [Bool] -> Int
fromPixels = foldl (\acc -> (+) (2*acc) . fromEnum) 0


pixel :: Image -> Coord -> Bool
pixel i (x,y) = i !! y !! x

square3x3 :: Image -> Coord -> [Bool]
square3x3 i (x,y)  = map (pixel i) $ [(x', y')  | y' <- [y-1..y+1], x' <- [x-1..x+1]]

onEdge :: Image -> Coord -> Bool
onEdge i (x,y)  = let (nRows,nCols) = size i
                  in x == 0 || x == pred nCols ||
                     y == 0 || y == pred nRows

size :: Image -> (Int,Int)
size i = (length i, length $ head i)

allCoords :: Image -> [[Coord]]
allCoords i = let (nRows,nCols) = size i
              in chunksOf nCols [(c-1,r-1) | r <- [1..nRows], c <- [1..nCols]]

isValidCoord :: Image -> Coord -> Bool
isValidCoord i (c,r)  = let (nRows,nCols) = size i in
                        c >= 0 && r >= 0 && c < nCols && r < nRows


-- Simulate an "infinite" image based on the input image
expandImage :: Image -> Image
expandImage i = let (nRows,nCols) = size i
                    buffer        = 102 -- sufficient infiniteness for 50 image enchancements
                    topAndBot     = replicate buffer $ replicate (2*buffer+nCols) False
                    rowSide       = replicate buffer False in
                topAndBot ++ map (\x -> rowSide ++ x ++ rowSide) i ++ topAndBot

-- Strip off the sacrifical edge so it doesn't taint our results
reduceImage :: Image -> Image
reduceImage i = let (nRows,nCols) = size i
                    buffer = 51 -- outer rows are garbage due to finite input
                    rowBuffer = nRows - buffer
                    colBuffer = nCols - buffer in
                map (drop buffer . take colBuffer) $ drop buffer (take rowBuffer i)



parseInput :: [String] -> (ImageEnhancementAlgorithm, Image)
parseInput (s:ss) = let (ieaString, iiString) = (s, tail ss) in
                    (M.fromList $ zip [0..] (map toPixel ieaString),
                     expandImage $ map (map toPixel) iiString)
parseInput _      = undefined


enhance :: (ImageEnhancementAlgorithm, Image) -> (ImageEnhancementAlgorithm, Image)
enhance (iea, i) = (iea, map (map expandPixel) $ allCoords i) where
  expandPixel coord
    | onEdge i coord  = pixel i coord
    | otherwise       = iea ! fromPixels (square3x3 i coord)


part1 :: [String] -> Int
part1 = length . concatMap (filter id) . reduceImage . snd . enhance . enhance . parseInput

part2 :: [String] -> Int
part2 inp = let (iea,i) = parseInput inp in
            length . concatMap (filter id) . reduceImage . snd $ (iterate enhance (iea,i) !! 50)
