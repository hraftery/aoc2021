module Main where

import Data.List (nub, elemIndex, find)
import Data.Maybe (isJust, fromJust, isNothing, mapMaybe, listToMaybe)
import Algorithm.Search (dijkstra)

import Debug.Trace (trace)

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

data Amphipod = A | B | C | D deriving (Show, Read, Eq, Ord) -- short for "AmphipodType"
type Hallway = [Maybe Amphipod] -- index 0 is left, 2,4,6,8 is outside roomA,B,C,D, 10 is right
type Room = (Maybe Amphipod, Maybe Amphipod, -- first is top, last is bottom
             Maybe Amphipod, Maybe Amphipod) -- second is upper middle, third is lower middle

data Situation = Situation
  { hallway :: Hallway
  , roomA   :: Room
  , roomB   :: Room
  , roomC   :: Room
  , roomD   :: Room
  } deriving (Show, Eq, Ord)

-- s = Situation { hallway = replicate 11 Nothing, roomA = (Just B, Nothing, Nothing, Just A), roomB = (Just C, Nothing, Nothing, Just D), roomC = (Just B, Nothing, Nothing, Just C), roomD = (Just D, Nothing, Nothing, Just A) }
-- s2= Situation { hallway = replicate 11 Nothing, roomA = (Just B, Just D,  Just D,  Just A), roomB = (Just C, Just C,  Just B,  Just D), roomC = (Just B, Just B,  Just A,  Just C), roomD = (Just D, Just A,  Just C,  Just A) }

type HallwayLoc = Int
type RoomLoc = (Amphipod, RoomPos)
data RoomPos = Top | UpMid | LoMid | Bottom deriving (Show, Eq, Ord)
data Location = InHallway HallwayLoc | InRoom RoomLoc deriving (Show, Eq, Ord)
type Move = (Location, Location) -- src, dst

emptyHallway :: Hallway
emptyHallway = replicate 11 Nothing

emptyRoom :: Room
emptyRoom = (Nothing, Nothing, Nothing, Nothing)

emptySituation :: Situation
emptySituation = Situation { hallway = emptyHallway,  roomA = emptyRoom,
                                                      roomB = emptyRoom,
                                                      roomC = emptyRoom,
                                                      roomD = emptyRoom }

allHallwayLocs :: [Location]
allHallwayLocs =  [InHallway i | i<-[0..10]]

allRoomLocs :: [Location]
allRoomLocs = [InRoom (a,p) | p <- [Top, UpMid, LoMid, Bottom], a <- [A, B, C, D]]

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
amphipodAtRoomLoc Top         (f,_,_,_) = f
amphipodAtRoomLoc UpMid       (_,u,_,_) = u
amphipodAtRoomLoc LoMid       (_,_,l,_) = l
amphipodAtRoomLoc Bottom      (_,_,_,b) = b


parseInput :: [String] -> Situation
parseInput ls = let top = parseRoomLine $ ls !! 2
                    bot = parseRoomLine $ ls !! 3
                in Situation { hallway = replicate 11 Nothing,
                               roomA   = (top !! 0, Nothing, Nothing, bot !! 0),
                               roomB   = (top !! 1, Nothing, Nothing, bot !! 1),
                               roomC   = (top !! 2, Nothing, Nothing, bot !! 2),
                               roomD   = (top !! 3, Nothing, Nothing, bot !! 3) }

parseRoomLine :: String -> [Maybe Amphipod]
parseRoomLine l = map (Just . read . (\i -> [l !! i])) [3, 5, 7, 9]

amphipodLocations :: Situation -> [Location]
amphipodLocations s = filter (isJust . amphipodAtLocation s) allLocations

allMovesFrom :: Situation -> Location -> [Location]
allMovesFrom s l = f s l (fromJust $ amphipodAtLocation s l) where
  f s l@(InHallway _) a  = pathsToHome s l a -- an amphipod in hallway is in want of its room
  f s l               a  = pathsFromHome s l a

allMovesFrom2 :: Situation -> Location -> [Location]
allMovesFrom2 s l = f s l (fromJust $ amphipodAtLocation s l) where
  f s l@(InHallway _) a  = pathsToHome2 s l a -- an amphipod in hallway is in want of its room
  f s l               a  = pathsFromHome2 s l a

isPath :: Situation -> Location -> Location -> Bool
isPath s l1                   l2
  | isJust $ amphipodAtLocation s l2          = False
isPath s l1@(InRoom (_,Top))  l2              = isPathRecurse s (entrance l1)       l2
isPath s (InRoom (r,UpMid))   l2              = isPathRecurse s (InRoom (r,Top))    l2
isPath s (InRoom (r,LoMid))   l2              = isPathRecurse s (InRoom (r,UpMid))  l2
isPath s (InRoom (r,Bottom))  l2              = isPathRecurse s (InRoom (r,LoMid))  l2
isPath s l1@(InHallway h1)    l2@(InHallway h2)
  | h1 == h2                                  = True
  | otherwise                                 = isPathRecurse s (nextTowardsH l1 l2) l2
isPath s l1@(InHallway h1)    l2@(InRoom (a,tb))
  | l1 == entrance l2                         = tb == Top ||
                                                (isNoAmphipodAtLocation s (InRoom (a,Top))   && (tb == UpMid ||
                                                (isNoAmphipodAtLocation s (InRoom (a,UpMid)) && (tb == LoMid ||
                                                 isNoAmphipodAtLocation s (InRoom (a,LoMid))))))
  | otherwise                                 = isPathRecurse s (nextTowardsH l1 $ entrance l2) l2

isPathRecurse :: Situation -> Location -> Location -> Bool
isPathRecurse s l1 l2 = isNoAmphipodAtLocation s l1 && isPath s l1 l2

entrance :: Location -> Location
entrance (InRoom (A,_))   = InHallway 2
entrance (InRoom (B,_))   = InHallway 4
entrance (InRoom (C,_))   = InHallway 6
entrance (InRoom (D,_))   = InHallway 8
entrance _                = undefined

allEntrances :: [Location]
allEntrances = [entrance l | l <- map InRoom [(A,Top), (B,Top), (C,Top), (D,Top)]]

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

pathsToHome2 :: Situation -> Location -> Amphipod -> [Location]
pathsToHome2 s l a  = let [homeBot, homeLoMid, homeUpMid, homeTop] = [InRoom (a,l) | l <- [Bottom, LoMid, UpMid, Top]] in
                      case map (isPath s l) [homeBot, homeLoMid, homeUpMid, homeTop] of
                        [True, _, _, _] -> [homeBot]
                        [_, True, _, _] -> [homeLoMid | amphipodAtLocation s homeBot == Just a]
                        [_, _, True, _] -> [homeUpMid | amphipodAtLocation s homeBot == Just a,
                                                        amphipodAtLocation s homeLoMid == Just a ]
                        [_, _, _, True] -> [homeTop   | amphipodAtLocation s homeBot == Just a,
                                                        amphipodAtLocation s homeLoMid == Just a,
                                                        amphipodAtLocation s homeUpMid == Just a ]
                        _               -> []

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

pathsFromHome2 :: Situation -> Location -> Amphipod -> [Location]
-- If we're already in our final spot, then no paths
pathsFromHome2 s (InRoom (r,Bottom)) a
  | r == a                              = []
pathsFromHome2 s (InRoom (r,LoMid))  a
  | r == a && amphipodAtLocation s (InRoom (r,Bottom)) == Just a
                                        = []
pathsFromHome2 s (InRoom (r,UpMid))    a
  | r == a && amphipodAtLocation s (InRoom (r,Bottom)) == Just a
           && amphipodAtLocation s (InRoom (r,LoMid)) == Just a
                                        = []
pathsFromHome2 s (InRoom (r,Top))    a
  | r == a && amphipodAtLocation s (InRoom (r,Bottom)) == Just a
           && amphipodAtLocation s (InRoom (r,LoMid)) == Just a
           && amphipodAtLocation s (InRoom (r,UpMid)) == Just a
                                        = []
pathsFromHome2 s l@(InRoom (r,_))    a
-- If we are in our room, but it is not our final spot, then can only move to hallway
  | r == a                              = pathsToHallway s l
-- Otherwise, could move to hallway or directly to our home
  | otherwise                           = pathsToHallway s l ++ pathsToHome2 s l a
pathsFromHome2 _ (InHallway _)       _  = undefined


pathsToHallway :: Situation -> Location -> [Location]
pathsToHallway s l@(InRoom _)   = filter (isPath s l) $ filter (`notElem` allEntrances) allHallwayLocs
pathsToHallway _ _              = undefined

makeMove :: Situation -> Move -> Situation
makeMove s (lSrc, lDst) = modify lDst (amphipodAtLocation s lSrc) $ modify lSrc Nothing s

modify :: Location -> Maybe Amphipod -> Situation -> Situation
modify (InHallway i) a (Situation h ra rb rc rd)  = Situation {
                                                      hallway = take i h ++ [a] ++ drop (i+1) h,
                                                      roomA=ra, roomB=rb, roomC=rc, roomD=rd }
modify (InRoom (A,p)) a (Situation h ra rb rc rd) = let (r1, r2, r3, r4) = ra in
                                                    Situation {
                                                      roomA = case p of
                                                                Top     -> ( a, r2, r3, r4)
                                                                UpMid   -> (r1,  a, r3, r4)
                                                                LoMid   -> (r1, r2,  a, r4)
                                                                Bottom  -> (r1, r2, r3,  a),
                                                      hallway=h, roomB=rb, roomC=rc, roomD=rd }
modify (InRoom (B,p)) a (Situation h ra rb rc rd) = let (r1, r2, r3, r4) = rb in
                                                    Situation {
                                                      roomB = case p of
                                                                Top     -> ( a, r2, r3, r4)
                                                                UpMid   -> (r1,  a, r3, r4)
                                                                LoMid   -> (r1, r2,  a, r4)
                                                                Bottom  -> (r1, r2, r3,  a),
                                                      hallway=h, roomA=ra, roomC=rc, roomD=rd }
modify (InRoom (C,p)) a (Situation h ra rb rc rd) = let (r1, r2, r3, r4) = rc in
                                                    Situation {
                                                      roomC = case p of
                                                                Top     -> ( a, r2, r3, r4)
                                                                UpMid   -> (r1,  a, r3, r4)
                                                                LoMid   -> (r1, r2,  a, r4)
                                                                Bottom  -> (r1, r2, r3,  a),
                                                      hallway=h, roomA=ra, roomB=rb, roomD=rd }
modify (InRoom (D,p)) a (Situation h ra rb rc rd) = let (r1, r2, r3, r4) = rd in
                                                    Situation {
                                                      roomD = case p of
                                                                Top     -> ( a, r2, r3, r4)
                                                                UpMid   -> (r1,  a, r3, r4)
                                                                LoMid   -> (r1, r2,  a, r4)
                                                                Bottom  -> (r1, r2, r3,  a),
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

allMoves2 :: Situation -> [Move]
allMoves2 s = concatMap (\l -> zip (repeat l) (allMovesFrom2 s l)) $ amphipodLocations s

makeAllMoves :: Situation -> [(Move, Situation)]
makeAllMoves s = map (\m -> (m, makeMove s m)) $ allMoves s

makeAllMoves2 :: Situation -> [(Move, Situation)]
makeAllMoves2 s = map (\m -> (m, makeMove s m)) $ allMoves2 s

isOrganised :: Situation -> Bool
isOrganised (Situation h ra rb rc rd) = all isNothing h &&
                                        ra == (Just A, Nothing, Nothing, Just A) &&
                                        rb == (Just B, Nothing, Nothing, Just B) &&
                                        rc == (Just C, Nothing, Nothing, Just C) &&
                                        rd == (Just D, Nothing, Nothing, Just D)

isOrganised2 :: Situation -> Bool
isOrganised2 (Situation h ra rb rc rd)  = all isNothing h &&
                                          ra == (Just A, Just A, Just A, Just A) &&
                                          rb == (Just B, Just B, Just B, Just B) &&
                                          rc == (Just C, Just C, Just C, Just C) &&
                                          rd == (Just D, Just D, Just D, Just D)

score :: Move -> Amphipod -> Int
score m a = stepCount m * case a of { A -> 1; B -> 10; C -> 100; D -> 1000} where
  stepCount (s@(InHallway h), e@(InRoom (_,p)))   = let (InHallway ee) = entrance e in
                                                    abs(h-ee) + if p == Top then 1 else 2
  stepCount (s@(InRoom _),    e@(InHallway _))    = stepCount (e,s)
  stepCount (s@(InRoom _),    e@(InRoom _))       = stepCount (s,entrance e) +
                                                    stepCount (entrance e, e)
  stepCount _ = error "Invalid move."

score2 :: Move -> Amphipod -> Int
score2 m a = stepCount m * case a of { A -> 1; B -> 10; C -> 100; D -> 1000} where
  stepCount (s@(InHallway h), e@(InRoom (_,p)))   = let (InHallway ee) = entrance e in
                                                    abs(h-ee) + case p of
                                                                  Top     -> 1
                                                                  UpMid   -> 2
                                                                  LoMid   -> 3
                                                                  Bottom  -> 4
  stepCount (s@(InRoom _),    e@(InHallway _))    = stepCount (e,s)
  stepCount (s@(InRoom _),    e@(InRoom _))       = stepCount (s,entrance e) +
                                                    stepCount (entrance e, e)
  stepCount _ = error "Invalid move."

scoreMove :: Situation -> Move -> Int
scoreMove s m = let a = fromJust $ amphipodAtLocation s (fst m)
                in score m a

scoreMove2 :: Situation -> Move -> Int
scoreMove2 s m  = let a = fromJust $ amphipodAtLocation s (fst m)
                  in score2 m a

scorePath :: Situation -> [Move] -> Int
scorePath s = snd . foldl moveAndScore (s, 0) where
  moveAndScore (s', scoreAcc) m = (makeMove s' m, scoreAcc + scoreMove s' m)

leastEnergy :: Situation -> Maybe (Int, [Situation])
leastEnergy = dijkstra neighbours energyBetween isOrganised where
  neighbours            = map snd . makeAllMoves
  energyBetween from to = let edges = makeAllMoves from
                              (move,_) = fromJust $ find ((==to) . snd) edges
                          in scoreMove from move

leastEnergy2 :: Situation -> Maybe (Int, [Situation])
leastEnergy2 = dijkstra neighbours energyBetween isOrganised2 where
  neighbours            = map snd . makeAllMoves2
  energyBetween from to = let edges = makeAllMoves2 from
                              (move,_) = fromJust $ find ((==to) . snd) edges
                          in scoreMove2 from move


part1 :: [String] -> Int
part1 = fst . fromJust . leastEnergy . parseInput

part2 :: [String] -> Int
part2 = let extraRooms = [InRoom (a,p) | a <- [A, B, C, D], p <- [UpMid, LoMid]]
            extraAmphipods = map Just [D, D, C, B, B, A, A, C] in
        fst . fromJust . leastEnergy2 .
        modifyList extraRooms extraAmphipods . parseInput
