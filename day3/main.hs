module Main where

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)


mostCommon :: String -> Bool
mostCommon s = let numTot  = length s
                   numOnes = length $ filter (=='1') s
               in numOnes*2 >= numTot

mostCommonList :: [String] -> [Bool]
mostCommonList ss = if any null ss then []
                    else mostCommon (map head ss) : mostCommonList (map tail ss)

bitFieldToInt :: [Bool] -> Int
bitFieldToInt = foldl (\acc x -> acc*2 + fromEnum x) 0

part1 :: [String] -> (Int,Int)
part1 ss = let mc          = mostCommonList ss
               gammaRate   = bitFieldToInt mc
               epsilonRate = bitFieldToInt $ map not mc
           in (gammaRate, epsilonRate)


keepCommon :: [String] -> Bool -> [Bool]
keepCommon ss most = f ss most 0
  where
    f [s] _    _  = map (\x -> mostCommon [x]) s
    f ss  most i  = let keep = if most == mostCommon (map (!!i) ss) then '1' else '0'
                        ss'  = filter ((==keep) . (!!i)) ss
                    in f ss' most (succ i)

part2 :: [String] -> (Int, Int)
part2 ss = let o2rating   = bitFieldToInt $ keepCommon ss True
               co2rating  = bitFieldToInt $ keepCommon ss False
           in (o2rating, co2rating)
