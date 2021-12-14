module Main where

import Data.Char (isSpace)
import Data.List (sort, group, foldl')
import Data.List.Split (divvy)
import Data.Bifunctor (bimap, second)
import Data.Map  (Map, (!))
import qualified Data.Map as M

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

type PolymerTemplate = String
type Polymer = String
type PolymerPair = String
type Element = Char
type InsertionRules = Map PolymerPair Element

type ElementCount = Map Element Integer
--type Cache = Map (PolymerPair,Int) ElementCount
type Cache = Map PolymerPair ElementCount

range :: (Num a, Foldable t, Ord a) => t a -> a
range l = maximum l - minimum l


parseInput :: [String] -> (PolymerTemplate,InsertionRules)
parseInput = bimap makeTemplate makeRules . break' (all isSpace) where
  break'        = (second tail .) . break -- chop the delimiter out of snd
  makeTemplate  = head
  makeRules     = (M.fromList .) . map $ second last . break' (==' ')

doStep :: InsertionRules -> Polymer -> Polymer
doStep rules poly = head poly : doTail poly where
  doTail = concatMap (tail . applyRule rules) . divvy 2 1
-- without applyRule:
--  doTail = concatMap (\[f,l] -> [rules ! [f,l], l]) . divvy 2 1

doSteps :: Int -> InsertionRules -> PolymerTemplate -> Polymer
doSteps n rules p = iterate (doStep rules) p !! n

elementCountRange :: Polymer -> Int
elementCountRange = range . map length . group . sort


applyRule :: InsertionRules -> PolymerPair -> Polymer
applyRule rules p@[f,l] = [f, rules ! p, l]
applyRule _ _ = undefined

{- Creates cache containing every step. Works but then I realised only the last step need be kept.
   Left for posterity.
makeCache :: InsertionRules -> Int -> Cache
makeCache rules maxSteps  = let cacheKeys = [(p,n') | n' <- [1..maxSteps], p <- M.keys rules]
                            in foldl' addToCache M.empty cacheKeys where
  addToCache cache (p,steps)  = M.insert (p,steps) val cache where
    val = let p'@[f,m,l]    = doStep rules p --note applyRule would do because p is pair, but choose to be generic intead
              firstStep     = zip p' [1,1,1]                      -- :: ElementCount, as list, potentially with duplicate keys
              prevStep1     = cache ! ([f,m], steps-1)            -- :: ElementCount
              prevStep2     = cache ! ([m,l], steps-1)            -- :: ElementCount
              sumPrevSteps  = M.unionWith (+) prevStep1 prevStep2 -- :: ElementCount
              middleOverlap = M.singleton m 1 in                  -- :: ElementCount
          if steps == 1 then M.fromListWith (+) firstStep --prime with results of first step
          else M.unionWith (-) sumPrevSteps middleOverlap --every other step is sum of two previous steps, minus the overlap
-}

makeCache :: InsertionRules -> Int -> Cache
makeCache rules steps   = iterate (stepCache rules) initialCache !! (steps-1) where
  initialCache            = foldr insertInitialCount M.empty $ M.keys rules
  insertInitialCount p    = M.insert p (M.fromListWith (+) $ zip (doStep rules p) [1,1,1])

stepCache :: InsertionRules -> Cache -> Cache
stepCache rules cache = foldr insertSteppedCount M.empty $ M.keys cache where
  insertSteppedCount p  = M.insert p (countFor p)
  countFor p            = let [f,m,l] = doStep rules p in
                          disjunctive (cache ! [f,m]) (cache ! [m,l]) m

disjunctive :: ElementCount -> ElementCount -> Element -> ElementCount
disjunctive a b overlap = M.unionWith (-) (M.unionWith (+) a b) (M.singleton overlap 1)

elementCountRange' :: ElementCount -> Integer
elementCountRange' = range . M.elems

doStepsUsingCache :: Int -> PolymerTemplate -> InsertionRules -> ElementCount
doStepsUsingCache steps t r = let cache   = makeCache r steps
                                  (p:ps)  = divvy 2 1 t in
                              foldl' (addCountFromCache cache) (cache ! p) ps where
  addCountFromCache cache counts pair = disjunctive counts (cache ! pair) (head pair)


part1 :: [String] -> Int
part1 = elementCountRange . uncurry (flip (doSteps 10)) . parseInput

part2 :: [String] -> Integer
part2 = elementCountRange' . uncurry (doStepsUsingCache 40) . parseInput
