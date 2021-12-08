module Main where
import Data.List (sort, (\\), intersect, sortBy, union)
import Data.Maybe (fromJust)
import Data.Map  (Map, (!))
import qualified Data.Map as M

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

type Pattern = [Signal]
type Digit = [Signal]
data Entry = Entry [Pattern] [Digit]

type Signal = Char
type Segment = Char
type SignalToSegmentMap = Map Signal Segment

allSigSegs :: [Char]
allSigSegs = "abcdefg"


parseInputLine :: String -> Entry
parseInputLine inp  = let ws = words inp
                      in Entry (takeWhile (/="|") ws) (tail $ dropWhile (/="|") ws)

numUniqueDigitsInOutput :: Entry -> Int
numUniqueDigitsInOutput (Entry _ output) = length $ filter isUniqueDigit output

isUniqueDigit :: Digit -> Bool
isUniqueDigit d = let l = length d in l `elem` [2,3,4,7] -- '1','7','4','8'

outputValue :: Entry -> Int
outputValue (Entry patterns output) = let m = findSignalMap patterns
                                      in foldl (\acc d -> acc*10 + digitToValue m d) 0 output

findSignalMap :: [Pattern] -> SignalToSegmentMap
findSignalMap pats  = let segA = l3 \\ l2
                          segG = (il6 \\ l4) \\ l3
                          segD = (il5 \\ segA) \\ segG
                          segB = (l4 \\ segD) \\ l2
                          segF = intersect il6 l2
                          segE = ((allSigSegs \\ l4) \\ segA) \\ segG
                          segC = allSigSegs \\ foldr1 union [segA,segB,segD,segE,segF,segG]
                          segmentToSignalMap = map head [segA,segB,segC,segD,segE,segF,segG]
                      in M.fromList $ zip segmentToSignalMap allSigSegs
  where
    l2  = head $ filter (\p -> length p == 2) pats  -- '1' (unique)
    l3  = head $ filter (\p -> length p == 3) pats  -- '7' (unique)
    l4  = head $ filter (\p -> length p == 4) pats  -- '4' (unique)
    l5  = filter (\p -> length p == 5) pats         -- '2' or '3' or '5'
    l6  = filter (\p -> length p == 6) pats         -- '6' or '9' or '0'
    il5 = foldr1 intersect l5                       -- common to '2' or '3' or '5'
    il6 = foldr1 intersect l6                       -- common to '6' or '9' or '0'

{- abondoned attempt to heuristically whittle the solution down. Let as comment for posterity.
findSignalMap pats = rule69 . rule4 . rule7 $ rule1 allPossibilities where
  allPossibilities        = M.fromList $ zip allSignals (repeat allSignals)
  ruleOutUniques pat m
    | length pat == 2     = foldr (M.adjust (intersect "cf")) m pat     -- '1'
    | length pat == 3     = foldr (M.adjust (intersect "acf")) m pat    -- '7'
    | length pat == 4     = foldr (M.adjust (intersect "bcdf")) m pat   -- '4'
    | length pat == 5     = m                                           -- '2' or '3' or '5'
    | length pat == 6     = m                                           -- '6' or '9' or '0'
    | length pat == 7     = m                                           -- '8'
    | otherwise           = error "Invalid pattern."
  rule1 m             = let pat  = head $ filter (\p -> length p == 2) pats
                            nPat = allSignals \\ pat
                            m'   = foldr (M.adjust (intersect "cf")) m  pat
                        in         foldr (M.adjust (\\ "cf"))        m' nPat
  rule7 m             = let pat  = head $ filter (\p -> length p == 3) pats
                            nPat = allSignals \\ pat
                            m'   = foldr (M.adjust (intersect "acf")) m  pat
                        in         foldr (M.adjust (\\ "acf"))        m' nPat
  rule4 m             = let pat  = head $ filter (\p -> length p == 4) pats
                            nPat = allSignals \\ pat
                            m'   = foldr (M.adjust (intersect "bcdf")) m  pat
                        in         foldr (M.adjust (\\ "bcdf"))        m' nPat
  rule69 m            = let pat2    = head $ filter (\p -> length p == 2) pats
                            pat6    = filter (\p -> length p == 6) pats
                            common  = foldr1 intersect $ pat2 : pat6
                            m'      = foldr (M.adjust (const "f")) m  common
                        in            foldr (M.adjust (\\ "f"))    m' (pat2 \\ common)
  rule235 m           = let pat2    = head $ filter (\p -> length p == 2) pats
                            pat5    = filter (\p -> length p == 5) pats
                            common  = foldr1 intersect $ pat2 : pat6
                            m'      = foldr (M.adjust (const "f")) m  common
                        in            foldr (M.adjust (\\ "f"))    m' (pat2 \\ common)
-}

digitToValue :: SignalToSegmentMap -> Digit -> Int
digitToValue m d  = case sort $ map (m !) d of
                      "abcefg"  -> 0
                      "cf"      -> 1
                      "acdeg"   -> 2
                      "acdfg"   -> 3
                      "bcdf"    -> 4
                      "abdfg"   -> 5
                      "abdefg"  -> 6
                      "acf"     -> 7
                      "abcdefg" -> 8
                      "abcdfg"  -> 9
                      _         -> error "Invalid digit."



part1 :: [String] -> Int
part1 = sum . map (numUniqueDigitsInOutput . parseInputLine)

part2 :: [String] -> Int
part2 = sum . map (outputValue . parseInputLine)
