module Main where
import Control.Monad (replicateM)


main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

type Score = Int
type Space = Int
type PlayerState = (Score, Space)


playRound :: (PlayerState, PlayerState, [Int]) -> (PlayerState, PlayerState, [Int])
playRound (p1, p2, die) = let (p1', die') = takeTurn (p1, die)
                              (p2', die'')= takeTurn (p2, die')
                          in if fst p1' >= 1000 then (p1', p2, die') else (p1', p2', die'')

takeTurn :: (PlayerState, [Int]) -> (PlayerState, [Int])
takeTurn ((score, space), die)  = let (this,rest) = splitAt 3 die
                                      space' = (space + sum this) `mod` 10
                                      score' = score + if space' == 0 then 10 else space'
                                  in ((score', space'), rest)

playGame :: (Int, Int) -> (PlayerState, PlayerState, Int)
playGame (space1, space2) = f ((0, space1), (0, space2)) 0 $ cycle [1..100] where
  f (p1, p2) n die  = let (p1', p2', die') = playRound (p1, p2, die) in
                      if fst p1' >= 1000 then (p1', p2', n*6 + 3)
                      else if fst p2' >= 1000 then (p1', p2', n*6 + 6)
                           else f (p1', p2') (succ n) die'


part1 :: [String] -> Int
part1 _ = let ((score1, space1), (score2, space2), n) = playGame (8, 9) in
          if score1 >= 1000 then n*score2 else n*score1


data Winner = P1 | P2 deriving (Eq, Show)

playRound2 :: (PlayerState, PlayerState, [Int]) -> (PlayerState, PlayerState, [Int])
playRound2 (p1, p2, die) = let (p1', die') = takeTurn2 (p1, die)
                               (p2', die'')= takeTurn2 (p2, die')
                           in if fst p1' >= 21 then (p1', p2, die') else (p1', p2', die'')

takeTurn2 :: (PlayerState, [Int]) -> (PlayerState, [Int])
takeTurn2 ((score, space), die)  = let (this,rest) = splitAt 3 die
                                       space' = (space + sum this) `mod` 10
                                       score' = score + if space' == 0 then 10 else space'
                                   in ((score', space'), rest)

takeTurn' :: PlayerState -> Int -> PlayerState
takeTurn' (score, space) roll = let space' = (space + roll) `mod` 10
                                    score' = score + if space' == 0 then 10 else space'
                                   in (score', space')

playGame2 :: (Int, Int, [Int]) -> Winner
playGame2 (space1, space2, die) = f ((0, space1), (0, space2)) die  where
  f (p1, p2) die  = let (p1', p2', die') = playRound2 (p1, p2, die) in
                    if fst p1' >= 21 then P1
                    else if fst p2' >= 21 then P2
                         else f (p1', p2') die'

winningRolls :: Score -> [[Int]]
winningRolls start = f (0,start) where
  f :: PlayerState -> [[Int]]
  f ps@(score, _)
    | score >= 21   = [[]]
    | otherwise     = [h:t | h <- [3..9], t <- f (takeTurn' ps h)]

winningRolls' :: Score -> Score -> [[Int]]
winningRolls' start1 start2 = f (0,start1) (0,start2) True where
  f :: PlayerState -> PlayerState -> Bool -> [[Int]]
  f ps1@(score1, _) ps2@(score2, _) turn
    | score1 >= 21  = [[]]
    | score2 >= 21  = [[]]
    | turn          = [h:t | h <- [3..9], t <- f (takeTurn' ps1 h) ps2 (not turn)]
    | otherwise     = [h:t | h <- [3..9], t <- f ps1 (takeTurn' ps2 h) (not turn)]

numGamesAndUniverses :: [[Int]] -> [(Int, Int)]
numGamesAndUniverses = map f where
  f rolls = let games = length rolls
                univs = product $ map waysToRoll rolls
            in (games, univs)

waysToRoll :: Int -> Int
waysToRoll n = length . filter ((==n) . sum) $ replicateM 3 [1,2,3] --replicateM == sequence $ replicate

totalGamesAndUniverses :: [(Int, Int)] -> [(Int, Int)]
totalGamesAndUniverses xs = let games = map fst xs
                                (min, max) = (minimum games, maximum games) in
                            [(g, u) | g <- [min..max], let u = sum . map snd $ filter ((==g) . fst) xs ]

totalWins :: [(Int, Int)] -> [(Int, Int)] -> Int
totalWins xs ys = sum [univs | x <- xs, y <- ys, fst x <= fst y, let univs = snd x * snd y]


-- less memory hungry version of `sequence $ take 10 (repeat [1,2,3])`
foo 0 _ = [[]]
foo k xs = [h:t | t <- foo (k-1) xs, h <- xs]


-- Turns out this takes too long, so instead I did:
--    wins8 = totalGamesAndUniverses . numGamesAndUniverses $ winningRolls 8
--    wins9 = totalGamesAndUniverses . numGamesAndUniverses $ winningRolls 9
-- effectively playing two single player games. Then I copied the results into
-- a spreadsheet which combines the two games by multiplying each number of
-- winning universes by the number of non-winning universes of the other player.
-- Converting that to Haskell sounds like more masochism that I can muster.
--part2 :: [String] -> Int
part2 _ = totalGamesAndUniverses . numGamesAndUniverses $ winningRolls' 4 8
