module Main where

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 contents)
  putStr "Part 2: "
  print (part2 contents)


parseInput :: String -> [Int]
parseInput = map read . words . map (\c -> if c==',' then ' ' else c)

doDay :: [Int] -> [Int]
doDay xs = let spawn  = length $ filter (==0) xs
               xs'    = map (\x -> if x == 0 then 6 else pred x) xs
           in xs' ++ replicate spawn 8

doDays :: Int -> [Int] -> [Int]
doDays n a = iterate doDay a !! n

part1 :: String -> Int
part1 = length . doDays 80 . parseInput


-- Now just store the counts of internal timer equal to 0, 1, 2, 3, 4, 5, 6, 7 and 8 
valuesToCounts :: [Int] -> [Integer]
valuesToCounts vs = map (\n -> fromIntegral . length $ filter (==n) vs) [0..8]

-- on counts, not values
doDay' :: [Integer] -> [Integer]
doDay' counts = let downCounts = map (counts !!) [1..8] ++ [0]
                    newCounts  = replicate 6 0 ++ [head counts, 0, head counts]
                in zipWith (+) downCounts newCounts

doDays' :: Int -> [Integer] -> [Integer]
doDays' n a = iterate doDay' a !! n

part2 :: String -> Integer
part2 = sum . doDays' 256 . valuesToCounts . parseInput
