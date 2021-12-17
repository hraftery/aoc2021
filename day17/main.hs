module Main where

import Data.List.Split (splitOn, wordsBy)
import Data.List (nub)


main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 contents)
  putStr "Part 2: "
  print (part2 contents)

data Target = Target { xlo::Int, xhi::Int, ylo::Int, yhi::Int }
toTarget :: [[Int]] -> Target
toTarget [[xlo,xhi],[ylo,yhi]] = Target xlo xhi ylo yhi
toTarget _ = undefined

data Solution = Solution { x::Int, y::Int, n::Int } deriving Show

-- Oh Haskell, you can be so pretty.
parseInput :: String -> Target
parseInput = toTarget . map (map (read . takeWhile (/=',')) . splitOn "..") . tail . wordsBy (=='=')

allSoln :: Target -> [Solution]
allSoln t = let Target xlo xhi ylo yhi = t in
            [Solution x y n |
              let ymax = if ylo < 0 then -1-ylo else yhi --safe to assume target doesn't include y=0
                  ymin = if ylo < 0 then ylo else 0, 
              y<-[ymax,pred ymax..ymin],
              let nmax = 2*y*y + 3*y + 1 - 4*ylo, -- conservative upper bound
              n<-[0..nmax], -- beyond this we're beyond the target for sure
              let yn = (n * (2*y-n+1)) `div` 2,
              yn >= ylo, yn <= yhi,
              x<-[1..xhi],
              let xn = if n<x then (n * (2*x-n+1)) `div` 2 else (x * (x+1)) `div` 2,
              xn >= xlo, xn <= xhi]

maxHeight :: Solution -> Int
maxHeight (Solution x y n) = (y * (y+1)) `div` 2

part1 :: String -> Int
part1 = maxHeight . head . allSoln . parseInput

part2 :: String -> Int
part2 = length . nub . map (\s -> (x s, y s)) . allSoln . parseInput

