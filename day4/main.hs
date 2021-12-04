module Main where
import Data.Char (isDigit, isSpace)
import Data.List (transpose)
import Data.List.Split (wordsBy)
import Data.Tuple.Extra (both)


main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

boardSize :: Int
boardSize = 5

type Draws = [Int]
type Board = [[Int]] -- boardSize x boardSize
data Part  = Part1 | Part2 deriving (Eq)

--tuple with list of numbers to draw and all the boards, in row and col format
parseInput :: [String] -> (Draws, [(Board, Board)])
parseInput []     = error "Input cannot be empty."
parseInput (l:ls) = (map read $ wordsBy (==',') l, parseBoards (tail ls))

parseBoards :: [String] -> [(Board,Board)]
parseBoards [] = []
parseBoards ls = let (thisBoard,boardSep) = span (any isDigit) ls
                     asBoard = map (map read . words) thisBoard
                     nextBoard = dropWhile (all isSpace) boardSep
                 in (asBoard, transpose asBoard) : parseBoards nextBoard

getWinner :: [Int] -> [(Board, Board)] -> (Int, Board)
getWinner [] _ = error "Need non-empty draws to find winner"
getWinner (d:ds) boards = let boards' = doDraw d boards
                              winners  = filter isWinningBoard boards' in
                          if null winners then getWinner ds boards'
                          else (d, fst $ head winners)

getLoser :: [Int] -> [(Board, Board)] -> (Int, Board)
getLoser [] _ = error "Need non-empty draws to find loser"
getLoser (d:ds) boards =  let boards' = doDraw d boards
                              losers  = filter (not . isWinningBoard) boards' in
                          if null losers then (d, fst $ head boards')
                          else getLoser ds losers

isWinningBoard :: (Board,Board) -> Bool
isWinningBoard (bByRow, bByCol) = any null bByRow || any null bByCol

doDraw :: Int -> [(Board,Board)] -> [(Board,Board)]
doDraw draw = map $ both $ map (filter (/=draw))

go :: [String] -> Part -> (Int,Int)
go ss p = let (draws, boards) = parseInput ss
              (draw,  winner) = (if p == Part1 then getWinner else getLoser) draws boards
          in (draw, sum $ map sum winner)

part1 :: [String] -> (Int,Int)
part1 ss = go ss Part1

part2 :: [String] -> (Int, Int)
part2 ss = go ss Part2
