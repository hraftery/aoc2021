module Day1 where

go :: [String] -> String
go = show . part2 . map read

part1 :: [Int] -> Int
part1 = snd . foldl (\(prev,count) x -> (x, count + if x > prev then 1 else 0)) (maxBound :: Int, 0)

part1' :: [Int] -> Int
part1' x = length . filter (==True) $ zipWith (>) x (tail x)

part2 :: [Int] -> Int
part2 = part1 . threes

threes :: [Int] -> [Int]
threes xs = sum (take 3 xs) : if length xs > 3 then threes (tail xs) else []
