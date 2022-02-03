module Main where

import Data.List (nub, elemIndex)
import Data.Maybe ( isJust, fromJust, isNothing, mapMaybe, listToMaybe )
import Debug.Trace (trace)
import Data.Set  (Set)
import qualified Data.Set as S
import qualified Data.Graph.Inductive as G

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

data Amphipod = A | B | C | D deriving (Show, Read, Eq, Ord) -- short for "AmphipodType"
type Hallway = [Maybe Amphipod] -- index 0 is left, 2,4,6,8 is outside roomA,B,C,D, 10 is right
type Room = (Maybe Amphipod, Maybe Amphipod) -- fst is top, snd is bottom

data Situation = Situation
  { hallway :: Hallway
  , roomA   :: Room
  , roomB   :: Room
  , roomC   :: Room
  , roomD   :: Room
  } deriving (Show, Eq, Ord)

type HallwayLoc = Int
type RoomLoc = (Amphipod, RoomPos)
data RoomPos = Top | Bottom deriving (Show, Eq, Ord)
data Location = InHallway HallwayLoc  | InRoom RoomLoc deriving (Show, Eq, Ord)
type Move = (Location, Location) -- src, dst

emptyHallway :: Hallway
emptyHallway = replicate 11 Nothing

emptyRoom :: Room
emptyRoom = (Nothing, Nothing)

emptySituation :: Situation
emptySituation = Situation { hallway = emptyHallway,  roomA = emptyRoom,
                                                      roomB = emptyRoom,
                                                      roomC = emptyRoom,
                                                      roomD = emptyRoom }

allHallwayLocs :: [Location]
allHallwayLocs =  [InHallway i | i<-[0..10]]

allRoomLocs :: [Location]
allRoomLocs = [InRoom (a,p) | p <- [Top, Bottom], a <- [A, B, C, D]]

allLocations :: [Location]
allLocations  = allHallwayLocs ++ allRoomLocs

amphipodAtLocation :: Situation -> Location -> Maybe Amphipod
amphipodAtLocation s (InHallway l)    = hallway s !! l
amphipodAtLocation s (InRoom (A,l))   = amphipodAtRoomLoc l (roomA s)
amphipodAtLocation s (InRoom (B,l))   = amphipodAtRoomLoc l (roomB s)
amphipodAtLocation s (InRoom (C,l))   = amphipodAtRoomLoc l (roomC s)
amphipodAtLocation s (InRoom (D,l))   = amphipodAtRoomLoc l (roomD s)

isNoAmphipodAtLocation :: Situation -> Location -> Bool
isNoAmphipodAtLocation s = isNothing . amphipodAtLocation s

amphipodAtRoomLoc :: RoomPos -> Room -> Maybe Amphipod
amphipodAtRoomLoc Top     = fst
amphipodAtRoomLoc Bottom  = snd


parseInput :: [String] -> Situation
parseInput ls = let top = parseRoomLine $ ls !! 2
                    bot = parseRoomLine $ ls !! 3
                in Situation { hallway = replicate 11 Nothing,
                               roomA   = (top !! 0, bot !! 0),
                               roomB   = (top !! 1, bot !! 1),
                               roomC   = (top !! 2, bot !! 2),
                               roomD   = (top !! 3, bot !! 3) }

parseRoomLine :: String -> [Maybe Amphipod]
parseRoomLine l = map (Just . read . (\i -> [l !! i])) [3, 5, 7, 9]

amphipodLocations :: Situation -> [Location]
amphipodLocations s = filter (isJust . amphipodAtLocation s) allLocations

allMovesFrom :: Situation -> Location -> [Location]
allMovesFrom s l = f s l (fromJust $ amphipodAtLocation s l) where
  f s l@(InHallway _) a  = pathsToHome s l a -- an amphipod in hallway is in want of its room
  f s l               a  = pathsFromHome s l a

isPath :: Situation -> Location -> Location -> Bool
isPath s l1                   l2
  | isJust $ amphipodAtLocation s l2          = False
isPath s l1@(InRoom (_,Top))  l2              = isPathRecurse s (entrance l1)     l2
isPath s (InRoom (r,Bottom))  l2              = isPathRecurse s (InRoom (r,Top))  l2
isPath s l1@(InHallway h1)    l2@(InHallway h2)
  | h1 == h2                                  = True
  | otherwise                                 = isPathRecurse s (nextTowardsH l1 l2) l2
isPath s l1@(InHallway h1)    l2@(InRoom (a,tb))
  | l1 == entrance l2                         = tb == Top || isNoAmphipodAtLocation s (InRoom (a,Top))
  | otherwise                                 = isPathRecurse s (nextTowardsH l1 $ entrance l2) l2

isPathRecurse :: Situation -> Location -> Location -> Bool
isPathRecurse s l1 l2 = isNoAmphipodAtLocation s l1 && isPath s l1 l2

entrance :: Location -> Location
entrance (InRoom (A,_))   = InHallway 2
entrance (InRoom (B,_))   = InHallway 4
entrance (InRoom (C,_))   = InHallway 6
entrance (InRoom (D,_))   = InHallway 8
entrance _                = undefined

nextTowards :: Int -> Int -> Int
nextTowards a b = if b > a then succ a else pred a

nextTowardsH :: Location -> Location -> Location
nextTowardsH (InHallway h1) (InHallway h2)  = InHallway $ nextTowards h1 h2
nextTowardsH _ _                            = undefined

pathsToHome :: Situation -> Location -> Amphipod -> [Location]
pathsToHome s l a = let [homeBot, homeTop] = [InRoom (a,l) | l <- [Bottom, Top]] in
                    case map (isPath s l) [homeBot, homeTop] of
                      [True, _] -> [homeBot]
                      [_, True] -> [homeTop | amphipodAtLocation s homeBot == Just a]
                      _         -> []


pathsFromHome :: Situation -> Location -> Amphipod -> [Location]
-- If we're already in our final spot, then no paths
pathsFromHome s (InRoom (r,Bottom)) a
  | r == a                              = []
pathsFromHome s (InRoom (r,Top))    a
  | r == a && amphipodAtLocation s (InRoom (r,Bottom)) == Just a
                                        = []
pathsFromHome s l@(InRoom (r,_))    a
-- If we are in our room, but it is not our final spot, then can only move to hallway
  | r == a                              = pathsToHallway s l
-- Otherwise, could move to hallway or directly to our home
  | otherwise                           = pathsToHallway s l ++ pathsToHome s l a
pathsFromHome _ (InHallway _)       _   = undefined

pathsToHallway :: Situation -> Location -> [Location]
pathsToHallway s l@(InRoom _)   = filter (isPath s l) $ filter (/= entrance l) allHallwayLocs
pathsToHallway _ _              = undefined

makeMove :: Situation -> Move -> Situation
makeMove s (lSrc, lDst) = modify lDst (amphipodAtLocation s lSrc) $ modify lSrc Nothing s

modify :: Location -> Maybe Amphipod -> Situation -> Situation
modify (InHallway i) a (Situation h ra rb rc rd)  = Situation {
                                                      hallway = take i h ++ [a] ++ drop (i+1) h,
                                                      roomA=ra, roomB=rb, roomC=rc, roomD=rd }
modify (InRoom (A,p)) a (Situation h ra rb rc rd) = Situation {
                                                      roomA = if p == Top then (a, snd ra)
                                                                          else (fst ra, a),
                                                      hallway=h, roomB=rb, roomC=rc, roomD=rd }
modify (InRoom (B,p)) a (Situation h ra rb rc rd) = Situation {
                                                      roomB = if p == Top then (a, snd rb)
                                                                          else (fst rb, a),
                                                      hallway=h, roomA=ra, roomC=rc, roomD=rd }
modify (InRoom (C,p)) a (Situation h ra rb rc rd) = Situation {
                                                      roomC = if p == Top then (a, snd rc)
                                                                          else (fst rc, a),
                                                      hallway=h, roomA=ra, roomB=rb, roomD=rd }
modify (InRoom (D,p)) a (Situation h ra rb rc rd) = Situation {
                                                      roomD = if p == Top then (a, snd rd)
                                                                          else (fst rd, a),
                                                      hallway=h, roomA=ra, roomB=rb, roomC=rc }

modifyList :: [Location] -> [Maybe Amphipod] -> Situation -> Situation
modifyList ls as s = foldr (\(l,a) acc -> modify l a acc) s $ zip ls as

allSituations :: [Situation]
allSituations = [modifyList           [la1,la2,lb1,lb2,lc1,lc2,ld1,ld2]
                            (map Just [A,  A,  B,  B,  C,  C,  D,  D]) emptySituation
                | la1 <- allLocations,
                  la2 <- filter (`notElem` [la1]) allLocations,
                  la2 > la1,
                  lb1 <- filter (`notElem` [la1, la2]) allLocations,
                  lb2 <- filter (`notElem` [la1, la2, lb1]) allLocations,
                  lb2 > lb1,
                  lc1 <- filter (`notElem` [la1, la2, lb1, lb2]) allLocations,
                  lc2 <- filter (`notElem` [la1, la2, lb1, lb2, lc1]) allLocations,
                  lc2 > lc1,
                  ld1 <- filter (`notElem` [la1, la2, lb1, lb2, lc1, lc2]) allLocations,
                  ld2 <- filter (`notElem` [la1, la2, lb1, lb2, lc1, lc2, ld1]) allLocations,
                  ld2 > ld1]

allMoves :: Situation -> [Move]
allMoves s = concatMap (\l -> zip (repeat l) (allMovesFrom s l)) $ amphipodLocations s

makeAllMoves :: Situation -> [(Move, Situation)]
makeAllMoves s = map (\m -> (m, makeMove s m)) $ allMoves s

isOrganised :: Situation -> Bool
isOrganised (Situation h ra rb rc rd) = all isNothing h &&
                                        ra == (Just A, Just A) && rb == (Just B, Just B) &&
                                        rc == (Just C, Just C) && rd == (Just D, Just D)

allSolutions :: Situation -> [[Move]]
allSolutions sOrig = go sOrig [] S.empty where
  go s mHist seen
    | scorePath sOrig (reverse mHist) >= 15730 = []
    | isOrganised s = [reverse mHist]
--    | length mHist >= 11 = []
    | otherwise     = concatMap goAgain (makeAllMoves s) where -- tbh, I still don't understand how "concat" helps
--    | otherwise     = concatMap (\(m, s') -> go s' (m:mHist)) (trace (show (length mHist) ++ show s) makeAllMoves s)
    goAgain (m,s')
      | S.member s' seen  = []
      | otherwise         = go s' (m:mHist) (S.insert s' seen)

toGraph :: G.Gr Situation Int
toGraph = let nodes = zip [1..] allSituations :: [G.LNode Situation]
              edges = [(fst src,
                        fromJust $ elemIndex (snd dst) (map snd nodes),
                        score (fst dst) (fromJust $ amphipodAtLocation (snd src) (fst $ fst dst))) |
                          src <- nodes,
                          dst <- makeAllMoves (snd src)] :: [G.LEdge Int]
              in G.mkGraph nodes edges

score :: Move -> Amphipod -> Int
score m a = stepCount m * case a of { A -> 1; B -> 10; C -> 100; D -> 1000} where
  stepCount (s@(InHallway h), e@(InRoom (_,p)))   = let (InHallway ee) = entrance e in
                                                    abs(h-ee) + if p == Top then 1 else 2
  stepCount (s@(InRoom _),    e@(InHallway _))    = stepCount (e,s)
  stepCount (s@(InRoom _),    e@(InRoom _))       = stepCount (s,entrance e) +
                                                    stepCount (entrance e, e)
  stepCount _ = error "Invalid move."

scorePath :: Situation -> [Move] -> Int
scorePath s = snd . foldl moveAndScore (s, 0) where
  moveAndScore (s', scoreAcc) m = let a = fromJust $ amphipodAtLocation s' (fst m) in
                                  (makeMove s' m, scoreAcc + score m a)


part1 :: [String] -> String
part1 inp = let s = parseInput inp in
--            show . minimum . map (scorePath s) $ allSolutions s
            show $ G.spLength 1 10 toGraph

part2 :: [String] -> Integer
part2 = undefined
