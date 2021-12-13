module Main where

import Data.Char (isUpper, isLower)
import Data.List.Split (wordsBy)
import Data.Map  (Map, (!))
import qualified Data.Map as M
import Data.List (nub)

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)


type Cave       = String
type CaveSystem = Map Cave [Cave]

isBig :: Cave -> Bool
isBig = isUpper . head

isSmall :: Cave -> Bool
isSmall = not . isBig

parseInput :: [String] -> CaveSystem
parseInput = M.fromListWith (++) . concatMap (makeCavePair . wordsBy (=='-')) where
  makeCavePair [from,to]  = [(from, [to]), (to, [from])]
  makeCavePair _          = error "Unexpected cave pair input"

allPathsFrom :: (Cave -> [Cave] -> Bool) -> Cave -> CaveSystem -> [[Cave]]
allPathsFrom pathGuard from sys = map (from:) (go from []) where
  go :: Cave -> [Cave] -> [[Cave]]
  go from soFar = let soFar' = from:soFar in
                  [next : rest |  next <- sys ! from, pathGuard next soFar',
                                  rest <- if next == "end" then [[]]
                                          else go next soFar']

pathGuard1 :: Cave -> [Cave] -> Bool
pathGuard1 next soFar = isBig next || next `notElem` soFar

pathGuard2 :: Cave -> [Cave] -> Bool
pathGuard2 next soFar = let l = filter isSmall soFar in
                        pathGuard1 next soFar ||
                        (next /= "start" && length (nub l) == length l)

part1 :: [String] -> Int
part1 = length . allPathsFrom pathGuard1 "start" . parseInput

part2 :: [String] -> Int
part2 = length . allPathsFrom pathGuard2 "start" . parseInput
