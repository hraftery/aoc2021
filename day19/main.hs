module Main where

import Data.List (transpose, foldl', tails)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.List.Split (splitOn)

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)


data Coord = Coord { x::Int, y::Int, z::Int } deriving (Show, Eq, Ord)
type Beacon   = Coord
type Beacons  = Set Beacon
type Scanner  = Set Beacon
type Scanners = Map Int Scanner

instance Num Coord where
  (Coord a b c) + (Coord x y z) = Coord (a+x) (b+y) (c+z)
  (Coord a b c) - (Coord x y z) = Coord (a-x) (b-y) (c-z)
  (Coord a b c) * (Coord x y z) = Coord (a*x) (b*y) (c*z)
  abs p         = error "not implemented"
  signum p      = error "not implemented"
  fromInteger p = error "not implemented"


parseInput :: [String] -> Scanners
parseInput = makeScannerMap . map (S.fromList . parseCoords) . splitOn [""]
  where
    parseCoords     = map parseCoord . drop 1
    parseCoord p    = let (x, y, z) = read $ "(" ++ p ++ ")" in Coord x y z
    makeScannerMap  = M.fromList . zip [0..]


allOrientations :: Coord -> [Coord]
allOrientations (Coord x y z)
  =[Coord   x    y    z ,  Coord   x  (-z)   y ,  Coord   x  (-y) (-z),  Coord   x    z  (-y),
    Coord   y    z    x ,  Coord   y  (-x)   z ,  Coord   y  (-z) (-x),  Coord   y    x  (-z),
    Coord   z    x    y ,  Coord   z  (-y)   x ,  Coord   z  (-x) (-y),  Coord   z    y  (-x),
    Coord (-x)   z    y ,  Coord (-x) (-y)   z ,  Coord (-x) (-z) (-y),  Coord (-x)   y  (-z),
    Coord (-y)   x    z ,  Coord (-y) (-z)   x ,  Coord (-y) (-x) (-z),  Coord (-y)   z  (-x),
    Coord (-z)   y    x ,  Coord (-z) (-x)   y ,  Coord (-z) (-y) (-x),  Coord (-z)   x  (-y)]

allScannerOrientations :: Scanner -> [Scanner]
allScannerOrientations = map S.fromList . transpose . map allOrientations . S.toList

findMatches :: Scanner -> Scanner -> Beacons
findMatches s1 s2 = if null unmatched then S.empty else head unmatched where
  unmatched  = [unmatched | c1  <- S.toList s1,
                            s2' <- allScannerOrientations s2,
                            c2  <- S.toList s2',
                            let shift = c1 - c2,
                            let shifted = S.map (+shift) s2',
                            let matches = s1 `S.intersection` shifted,
                            let unmatched = shifted \\ matches,
                            length matches >= 12]

uniques :: Scanners -> Beacons
uniques ss = head . M.elems $ iterate uniquesPass ss !! 10 -- by good luck, that's enough

uniquesPass :: Scanners -> Scanners
uniquesPass ss  = let keyScanOrder = tail . tail . reverse . tails $ M.keys ss in
                  foldl' f ss keyScanOrder where
  f m (k:ks)    = foldl' g ss ks where
    g m x       = let v = m ! k in
                  M.insert k (v `S.union` findMatches v (ss ! x)) m
  f _ _         = undefined
  

findDistance :: Scanner -> Scanner -> Coord
findDistance s1 s2 = if null dist then error "Must have overlap" else head dist where
  dist       = [shift     | c1  <- S.toList s1,
                            s2' <- allScannerOrientations s2,
                            c2  <- S.toList s2',
                            let shift = c1 - c2,
                            let shifted = S.map (+shift) s2',
                            let matches = s1 `S.intersection` shifted,
                            length matches >= 12]


part1 :: [String] -> Int
part1 = length . uniques . parseInput

part2 :: [String] -> Int
part2 inp = let allScanners = parseInput inp
                allBeaconsZeroOffset = uniques allScanners in 
            maximum [manhatten $ p1 - p2
                      | p1 <- map (findDistance allBeaconsZeroOffset) (M.elems allScanners),
                        p2 <- map (findDistance allBeaconsZeroOffset) (M.elems allScanners)]
  where
    manhatten (Coord x y z) = abs x + abs y + abs z

