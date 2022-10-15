module Main where

import Data.Char (digitToInt)
import Debug.Trace (trace)


main :: IO ()
main = do
  print run

run = let ins = filter (notElem '0') (map show [99999999999999,99999999999998..11111111111111])
          outs = map (\x -> (x, map digitToInt x)) ins
      in filter isGood outs
  where isGood (_,i) = if sum i == 50 then trace "Found one." True else False
