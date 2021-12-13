module Main where

import Data.Char (isSpace, toUpper)
import Data.Bifunctor (bimap, second)
import Data.List (nub)

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStrLn "Part 2: "
  mapM_ putStrLn (part2 $ lines contents)


data Axis = X | Y deriving Read
type Coord = (Int,Int)
type Fold = (Axis, Int)

parseInput :: [String] -> ([Coord],[Fold])
parseInput = bimap makeCoords makeFolds . break' (all isSpace) where
  break'      = (second tail .) . break -- chop the delimiter out of snd
  makeCoords  = map $ bimap read                      read . break' (==',')
  makeFolds   = map $ bimap (read.(:[]).toUpper.last) read . break' (=='=')

{-  Method without bimap abandoned but left for posterity. 
    Note: bimap f1 f2 (a,b) == (f1 a, f2 b) == (f1 *** f2) (a,b)
parseInput :: [String] -> ([Coord],[Fold])
parseInput inp = toPair $ zipWith ($) [makeCoords, makeFolds] (splitOn [""] inp) where
  makeCoords  = map (toPair . map read . splitOn ",")
  makeFolds   = map $ toPair . zipWith ($) [read.(:[]).toUpper.last, read] . splitOn "="

toPair :: [a] -> (a, a)
toPair [x,y]  = (x, y)
toPair _      = undefined
-}

applyFold :: Fold -> [Coord] -> [Coord]
applyFold f = nub . map (reflect f)

reflect :: Fold -> Coord -> Coord
reflect (X,v) (x,y) = (if x > v then 2*v - x else x, y)
reflect (Y,v) (x,y) = (x, if y > v then 2*v - y else y)


showPaper :: [Coord] -> [String]
showPaper cs  = let (maxX,maxY) = (maxOrd X cs, maxOrd Y cs) in
                [[if (x,y) `elem` cs then '#' else '.' | x<-[0..maxX]] | y<-[0..maxY]]

maxOrd :: Axis -> [Coord] -> Int
maxOrd axis = maximum . map (case axis of {X -> fst; Y -> snd})


part1 :: [String] -> Int
part1 inp = let (coords,folds) = parseInput inp in
            length $ applyFold (head folds) coords

part2 :: [String] -> [String]
part2 inp = let (coords,folds) = parseInput inp in
            showPaper $ foldl (flip applyFold) coords folds
