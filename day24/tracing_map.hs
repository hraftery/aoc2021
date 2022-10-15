module Main where

import Debug.Trace (trace)

main :: IO ()
main = do
  print $ run 5


doStuff i x = trace "Doing stuff." $ doStuffer 9 i x where
  doStuffer j i x = i * sum x + j

makeInput = map (5*)

run i = let ins = [[1,2,3,4], [5,6,7,8], [9,10,11,12]]
            outs = map (\x -> (x, doStuff i (makeInput x))) ins
        in outs
