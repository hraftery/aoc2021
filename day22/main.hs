module Main where

import Data.List.Split (splitOn, wordsBy)
import Data.Map  (Map)
import qualified Data.Map as M

import Data.Set  (Set)
import qualified Data.Set as S


main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

type Coord = (Int, Int, Int)
type RebootStep = (Bool, [Coord])
type Reactor = Map Coord Bool

type Interval = (Int, Int)
type Cuboid = (Interval, Interval, Interval)
type RebootStep' = (Bool, Cuboid)
type Reactor' = Set Cuboid


parseInputLine :: String -> RebootStep
parseInputLine l  = let (cmd, coords) = span (/=' ') l
                        [xRange, yRange, zRange] = map (drop 2) . wordsBy (==',') $ tail coords
                    in (cmd == "on", [(x,y,z) | x <- parseRange xRange, x >= -50, x <= 50,
                                                y <- parseRange yRange, y >= -50, y <= 50,
                                                z <- parseRange zRange, z >= -50, z <= 50])

parseRange :: String -> [Int]
parseRange s  = let [a,b] = map read $ splitOn ".." s
                in [a..b]


doStep :: Reactor -> RebootStep -> Reactor
doStep m (cmd,coords) = foldr (doFlip cmd) m coords

doFlip :: Bool -> Coord -> Reactor -> Reactor
doFlip cmd coord = M.insert coord cmd

doSteps :: [RebootStep] -> Reactor
doSteps = foldl doStep M.empty

countOns :: Reactor -> Int
countOns = M.foldr (\x a -> a + fromEnum x) 0

part1 :: [String] -> Int
part1 = countOns . doSteps . map parseInputLine



parseInputLine' :: String -> RebootStep'
parseInputLine' l = let (cmd, coords) = span (/=' ') l
                        [x, y, z] = map (parseRange' . drop 2) . wordsBy (==',') $ tail coords
                    in (cmd == "on", (x, y, z))

parseRange' :: String -> Interval
parseRange' s = let [a,b] = map read $ splitOn ".." s
                in (a,b)

doSteps' :: [RebootStep'] -> Reactor'
doSteps' = foldl doStep' S.empty

doStep' :: Reactor' -> RebootStep' -> Reactor'
doStep' r (True, cuboid) = addCuboidR r cuboid
doStep' r (False,cuboid) = removeCuboidR r cuboid


doesOverlapI :: Interval -> Interval -> Bool
doesOverlapI (a1, b1) (a2, b2) = a1 <= b2 && b1 >= a2

doesOverlapC :: Cuboid -> Cuboid -> Bool
doesOverlapC (x1, y1, z1) (x2, y2, z2)  = doesOverlapI x1 x2 &&
                                          doesOverlapI y1 y2 &&
                                          doesOverlapI z1 z2

isValidI :: Interval -> Bool
isValidI (a, b) = a <= b

isValidC :: Cuboid -> Bool
isValidC (x, y, z) = all isValidI [x, y, z]

-- Splits first interval into (before, during, after) second interval.
-- Note up to two of those sub-intervals may be invalid depending on the overlap.
splitInterval :: Interval -> Interval -> (Interval, Interval, Interval)
splitInterval (a1, b1) (a2, b2) = ((a1, a2-1), (max a1 a2, min b1 b2), (b2+1, b1))

removeCuboid :: Cuboid -> Cuboid -> Set Cuboid
removeCuboid (x1, y1, z1) (x2, y2, z2)  = let (xb, xd, xa) = splitInterval x1 x2
                                              (yb, yd, ya) = splitInterval y1 y2
                                              (zb, zd, za) = splitInterval z1 z2
                                              -- split into 6 possible sub-cuboids around the 2nd cuboid
                                              subCuboids = [(x1, y1, zb), -- 1 in front
                                                            (x1, yb, zd), -- 4 around, top
                                                            (x1, ya, zd), --           bottom
                                                            (xb, yd, zd), --           left
                                                            (xa, yd, zd), --           right
                                                            (x1, y1, za)] -- 1 behind
                                          in S.fromList $ filter isValidC subCuboids

removeCuboidR :: Reactor' -> Cuboid -> Reactor'
removeCuboidR r c = let overlaps = S.filter (doesOverlapC c) r in
                    if S.null overlaps then r
                    else S.foldr f r overlaps where
  f x acc = S.union (S.delete x acc) (removeCuboid x c)


addCuboid :: Cuboid -> Cuboid -> Set Cuboid
-- sneaky shortcut: a U b = a \ b + b
-- will produce more cuboids than necessary, but damn, so much easier to write
addCuboid c1 c2 = S.insert c2 $ removeCuboid c1 c2

addCuboidR :: Reactor' -> Cuboid -> Reactor'
addCuboidR r c  = S.insert c $ removeCuboidR r c

countOnsC :: Cuboid -> Integer
countOnsC (x,y,z) = product $ map (\(a,b) -> fromIntegral $ 1 + b - a) [x,y,z]

countOnsR :: Reactor' -> Integer
countOnsR = S.foldr (\x acc -> acc + countOnsC x) 0

part2 :: [String] -> Integer
part2 = countOnsR . doSteps' . map parseInputLine'
