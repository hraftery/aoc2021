module Main where
import Data.Maybe (fromJust, mapMaybe)
import Data.List (elemIndex, sort)
import Control.Monad (foldM)

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

openDelims :: String
openDelims  = "([{<"

closeDelims :: String
closeDelims = ")]}>"

toClose :: Char -> Char
toClose c = closeDelims !! fromJust (elemIndex c openDelims)

toScoreCorruption :: Char -> Int
toScoreCorruption c = [3, 57, 1197, 25137] !! fromJust (elemIndex c closeDelims)

toScoreCompletion :: Char -> Int
toScoreCompletion c = [1, 2, 3, 4] !! fromJust (elemIndex c closeDelims)


corruptionOrCompletion :: String -> Either Char String
corruptionOrCompletion = foldM f [] where
  f acc x   | x `elem` openDelims                 = Right $ toClose x : acc
            | not (null acc) && x == head acc     = Right $ tail acc
            | otherwise                           = Left x

firstCorruption :: String -> Maybe Char
firstCorruption = either Just (const Nothing) . corruptionOrCompletion

scoreCorruption :: Maybe Char -> Int
scoreCorruption Nothing   = 0
scoreCorruption (Just x)  = toScoreCorruption x

getCompletion :: String -> Maybe String
getCompletion = either (const Nothing) Just . corruptionOrCompletion

scoreCompletion :: String -> Int
scoreCompletion = foldl (\acc x -> 5*acc + toScoreCompletion x) 0

median :: [Int] -> Int
median xs = let n = length xs
            in sort xs !! (n `div` 2)


part1 :: [String] -> Int
part1 = foldr (\x acc -> acc + scoreCorruption (firstCorruption x)) 0

part2 :: [String] -> Int
part2 =  median . map scoreCompletion . mapMaybe getCompletion
