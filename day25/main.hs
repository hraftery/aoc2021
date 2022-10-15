module Main where

import Data.Char (toUpper, isDigit, digitToInt)
import Data.List (nub, elemIndex, find)
import Data.Maybe (isJust, fromJust, isNothing, mapMaybe, listToMaybe)

import Data.Set (Set, fromList, member, map)
import qualified Data.Set as Set

import Debug.Trace (trace)
import Text.Read (readMaybe)


main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  putStr (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

data Cell = East | South | Empty deriving (Eq)
type Coord = (Int, Int)
type Cells = Set Coord
data Map = Map { cols :: Int, rows :: Int, easts :: Cells, souths :: Cells } deriving Eq

instance Show Cell where
  show East   = ">"
  show South  = "v"
  show Empty  = "."

instance Read Cell where
  readsPrec d = readParen (d > 10) readsCell where
    readsCell :: ReadS Cell
    readsCell (x:rest) = [(case x of {'>' -> East; 'v' -> South; _ -> Empty}, rest)]

cellForMap :: Map -> Coord -> Cell
cellForMap m c = case (Set.member c (easts m), Set.member c (souths m)) of
                   (True, _) -> East
                   (_, True) -> South
                   _         -> Empty

instance Show Map where
  show m = unlines [[head . show $ cellForMap m (x,y) | x <- [0..(cols m)-1]] | y <- [0..(rows m)-1]]

makeMap :: [String] -> Map
makeMap ss =  let cs = length $ head ss
                  rs = length ss in
                Map {
                  cols    = cs,
                  rows    = rs,
                  easts   = Set.fromList $ [(x,y) | x <- [0..cs-1], y <- [0..rs-1], ss !! y !! x == '>'],
                  souths  = Set.fromList $ [(x,y) | x <- [0..cs-1], y <- [0..rs-1], ss !! y !! x == 'v']
                }

isEmpty :: Map -> Coord -> Bool
isEmpty m c = not (Set.member c (easts m) || Set.member c (souths m))

neighbour :: Map -> Coord -> Coord
neighbour m (x,y) = case cellForMap m (x,y) of
                      East  -> (succ x `mod` cols m, y)
                      South -> (x, succ y `mod` rows m)

moveIfPossible :: Map -> Coord -> Coord
moveIfPossible m c =  let c' = neighbour m c in
                      if isEmpty m c' then c' else c

step :: Map -> Map
step m =  let m' = Map {
            cols    = cols m,
            rows    = rows m,
            easts   = Set.map (moveIfPossible m) (easts m),
            souths  = souths m
          } in
          Map {
            cols    = cols m',
            rows    = rows m',
            easts   = easts m',
            souths  = Set.map (moveIfPossible m') (souths m')
          }

tryStep :: Map -> Maybe Map
tryStep m = let m' = step m in
            if m == m' then Nothing else Just m'


part1 :: [String] -> String
part1 inp = go (makeMap inp) 0 where
  go m i =  let i' = succ i
                m' = tryStep m
            in show m ++ "\n" ++ "After " ++ show i' ++ " steps:\n" ++ if isJust m' then go (fromJust m') i' else show $ step m


part2 :: [String] -> Int
part2 = undefined
