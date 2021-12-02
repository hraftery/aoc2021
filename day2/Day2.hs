module Day2 where

go = show . part2

data Direction = Forward | Down | Up
toDirection :: String -> Direction
toDirection "forward" = Forward
toDirection "down"    = Down
toDirection "up"      = Up
toDirection _         = error "Unexpected direction."

data Command = Command Direction Int
type Course = [Command]

makeCourse :: [String] -> Course
makeCourse = map (f . words) where
  f [d, i] = Command (toDirection d) (read i)
  f _ = error "Unexpected input."

part1 :: [String] -> (Int,Int)
part1 = foldl f (0,0) . makeCourse where
  f acc (Command d i) = case d of
                          Forward -> (fst acc + i, snd acc)
                          Down    -> (fst acc,     snd acc + i)
                          Up      -> (fst acc,     snd acc - i)

part2 :: [String] -> (Int,Int,Int)
part2 = foldl f (0,0,0) . makeCourse where
  f (h,d,a) (Command dir i) = case dir of
                                Forward -> (h+i, d+a*i, a)
                                Down    -> (h,   d,     a+i)
                                Up      -> (h,   d,     a-i)
