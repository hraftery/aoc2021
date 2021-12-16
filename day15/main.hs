module Main where

import Data.Char (digitToInt, ord, chr)
import Data.List (elemIndex)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import Data.Map  (Map, (!))
import qualified Data.Map as M

--import Dijkstra
--import Algorithm.Search
import qualified Data.Graph.Inductive as G


main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

type Coord        = (Int,Int)
type Level        = Int
type LevelGrid    = [[Level]]
type Cavern       = Map Coord (Level, [Coord])

toNeighbours :: LevelGrid -> Coord -> [Coord]
toNeighbours g (x,y)  = filter (isValidCoord g) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

size :: LevelGrid -> (Int,Int)
size g = (length g, length $ head g)

isValidCoord :: LevelGrid -> Coord -> Bool
isValidCoord g (x,y)  = let (nRows,nCols) = size g in
                        x >= 0 && y >= 0 && x < nCols && y < nRows

allCoords :: LevelGrid -> [[Coord]]
allCoords g = let (nRows,nCols) = size g
              in chunksOf nCols [(x-1,y-1) | y <- [1..nCols], x <- [1..nRows]]


parseInput :: [String] -> Cavern
parseInput inp  = let levelGrid   = map (map digitToInt) inp
                      coords      = concat $ allCoords levelGrid
                      levels      = concat levelGrid
                      neighbours  = map (toNeighbours levelGrid) coords in
                  M.fromList $ zip coords (zip levels neighbours)


getLevel :: Cavern -> Coord -> Level
getLevel cav coord = fst $ cav ! coord

getNeighbours :: Cavern -> Coord -> [Coord]
getNeighbours cav coord = snd $ cav ! coord

-- Problem space is too big for this to complete! Try Dijkstra instead.
allPathsFrom :: Coord -> Cavern -> [[Coord]]
allPathsFrom from cav = map (from:) (go from []) where
  go :: Coord -> [Coord] -> [[Coord]]
  go from' soFar  = let soFar' = from:soFar in
                    [next : rest |  next <- snd (cav ! from), next `notElem` soFar',
                                    getLevel cav next + pathRisk cav soFar' < 50,
                                    rest <- if next == (9,9) then [[]] --TODO: fix magic number
                                            else go next soFar']

pathRisk :: Cavern -> [Coord] -> Int
pathRisk cav = sum . map (getLevel cav)

{- Grr, Dijkstra.hs is too slow. Try FGL instead.
toGraph :: Cavern -> Graph
toGraph cav = M.elems $ M.mapWithKey makeNode cav where
  makeNode coord (_,coords) = (show coord, map makeEdge coords)
  makeEdge coord            = Edge (show coord) (fromIntegral $ getLevel cav coord)

part1 :: [String] -> Int
part1 inp = let cav = parseInput inp in
            sum . map (getLevel cav . read)
                . flip pathToNode "(99,99)" . flip dijkstra "(0,0)" $ toGraph cav

-}

toNode :: Cavern -> Coord -> Int
toNode cav coord = fromJust $ elemIndex coord (M.keys cav)

toGraph :: Cavern -> G.Gr String Int
toGraph cav = let coords  = M.keys cav
                  nodes   = map (\k -> (toNode cav k, show k)) coords :: [G.LNode String]
                  edges   = [(toNode cav src, toNode cav dst, getLevel cav dst) |
                              src <- coords,
                              dst <- getNeighbours cav src] :: [G.LEdge Int]
              in G.mkGraph nodes edges


-- Okay, things get progressively uglier from here on, so don't use this is as
-- inspiration for clean code. But I'm done with this day and just need to move on.

part1 :: [String] -> Int
part1 inp = let cav   = parseInput inp
                gr    = toGraph cav
                path  = fromJust $ G.sp 0 (M.size cav - 1) gr
                risk  = pathRisk cav $ map (\n -> fst $ M.elemAt n cav) path
            in risk - getLevel cav (0,0)

part2 :: [String] -> Int
part2 = part1 . expandInp

expandInp :: [[Char]] -> [[Char]]
expandInp inp = flatten [map applyShift ([x + y | x <- [0 .. 4]]) | y <- [0 .. 4]] where
  flatten           = concatMap (foldr1 (zipWith (++)))
  applyShift shift  = map (map (shiftChar shift)) inp
  shiftChar shift c = let c' = ord c - ord '1' in
                      chr $ ord '1' + ((c' + shift) `mod` length ['1'..'9'])
