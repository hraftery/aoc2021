module Main where

import Data.List (sort)

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 contents)
  putStr "Part 2: "
  print (part2 contents)


parseInput :: String -> [Int]
parseInput = map read . words . map (\c -> if c==',' then ' ' else c)

median :: [Int] -> Int
median xs = let n = length xs
            in sort xs !! (n `div` 2)

residuals :: Int -> [Int] -> [Int]
residuals m = map (\x -> abs(x-m))

residuals2 :: Int -> [Int] -> [Int]
residuals2 m = map (\x -> let n=abs(x-m) in (n*n+n) `div` 2)

part1 :: String -> Int
part1 inp = let xs = parseInput inp
            in sum $ residuals (median xs) xs

part2 :: String -> Int
part2 inp = let xs = parseInput inp
            in minimum $ map (\x -> sum $ residuals2 x xs) [1..999999]
