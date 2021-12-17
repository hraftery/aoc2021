module Main where

import Data.Bifunctor (first)
--import Control.Monad.State 

main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 contents)
  putStr "Part 2: "
  print (part2 contents)


type LiteralValue = Int
type Version      = Int
data TypeID       = LiteralValueID |
                    SumID | ProductID | MinimumID | MaximumID |
                    GreaterThanID | LessThanID | EqualToID deriving (Show, Eq)
toTypeID :: Int -> TypeID
toTypeID 4 = LiteralValueID
toTypeID 0 = SumID
toTypeID 1 = ProductID
toTypeID 2 = MinimumID
toTypeID 3 = MaximumID
toTypeID 5 = GreaterThanID
toTypeID 6 = LessThanID
toTypeID 7 = EqualToID
toTypeID _ = undefined
isOperatorID :: TypeID -> Bool
isOperatorID = (/=) LiteralValueID

data Packet       = Packet { version::Version, typeID::TypeID,
                             content::Either LiteralValue Packets } deriving Show
type Packets      = [Packet]

type Binary       = [Bool]
toBinary :: Char -> Binary
toBinary '0' = [False, False, False, False]
toBinary '1' = [False, False, False, True ]
toBinary '2' = [False, False, True,  False]
toBinary '3' = [False, False, True,  True ]
toBinary '4' = [False, True,  False, False]
toBinary '5' = [False, True,  False, True ]
toBinary '6' = [False, True,  True,  False]
toBinary '7' = [False, True,  True,  True ]
toBinary '8' = [True,  False, False, False]
toBinary '9' = [True,  False, False, True ]
toBinary 'A' = [True,  False, True,  False]
toBinary 'B' = [True,  False, True,  True ]
toBinary 'C' = [True,  True,  False, False]
toBinary 'D' = [True,  True,  False, True ]
toBinary 'E' = [True,  True,  True,  False]
toBinary 'F' = [True,  True,  True,  True ]
toBinary _   = undefined
toBinaryS :: String -> Binary
toBinaryS = concatMap toBinary
fromBinary :: Binary -> Int
fromBinary = foldl (\acc -> (+) (2*acc) . fromEnum) 0



parseVersion :: Binary -> (Version, Binary)
parseVersion = first fromBinary . splitAt 3

parseTypeID :: Binary -> (TypeID, Binary)
parseTypeID = first (toTypeID . fromBinary) . splitAt 3

parseLiteralValue :: Binary -> (LiteralValue, Binary)
parseLiteralValue b = first fromBinary $ go ([],b) where
  go :: (Binary, Binary) -> (Binary, Binary) -- I feel like the State monad should be helpful here, but can't see it.
  go (acc, next)  = let (this, rest) = splitAt 5 next
                        result = (acc ++ tail this, rest) in
                    (if head this then go else id) result

parseOperator :: Binary -> (Packets, Binary)
parseOperator (True:xs) = let (this, rest)  = splitAt 11 xs
                              numSubpackets = fromBinary this in
                          first reverse $ iterate parseNextPacket ([],rest) !! numSubpackets
parseOperator (False:xs)= let (this, rest)  = splitAt 15 xs
                              lenSubpacket = fromBinary this
                              (subpackets,beyond) = splitAt lenSubpacket rest in
                          first reverse (parseNextPackets ([],subpackets), beyond)
parseOperator []        = undefined

parsePacket :: Binary -> (Packet, Binary) -- Definitely State monad worthy, but eh, this works.
parsePacket b = let (version,b') = parseVersion b
                    (typeID,b'') = parseTypeID b'
                    (lv,b''')    = parseLiteralValue b''
                    (ps,b'''')   = parseOperator b'' in
                case typeID of
                  LiteralValueID  -> (Packet version typeID (Left lv),  b''')
                  _               -> (Packet version typeID (Right ps), b'''')

-- Yeah, these next two are super subtle. Couldn't come up with an abstraction
-- that merged them. Tread carefully.
parseNextPacket :: (Packets, Binary) -> (Packets, Binary)
parseNextPacket (packets,b)    = first (:packets) (parsePacket b)

parseNextPackets :: (Packets, Binary) -> Packets
parseNextPackets (packets,[])   = packets
parseNextPackets (packets,b)    = parseNextPackets $ parseNextPacket (packets,b)


versionSum :: Packet -> Int
versionSum p  = version p + either (const 0) (sum . map versionSum) (content p)


evaluatePacket :: Packet -> Int
evaluatePacket p = either id (applyOperator (typeID p) . map evaluatePacket) (content p)

-- Eh, was crying out to be pointfree in the second argument, but all cases need same
-- number of arguments, and doing the comparisons without patching matching would be blurgh.
applyOperator :: TypeID -> [Int] -> Int
applyOperator SumID         xs    = sum xs
applyOperator ProductID     xs    = product xs
applyOperator MinimumID     xs    = minimum xs
applyOperator MaximumID     xs    = maximum xs
applyOperator GreaterThanID [a,b] = fromEnum (a>b)
applyOperator LessThanID    [a,b] = fromEnum (a<b)
applyOperator EqualToID     [a,b] = fromEnum (a==b)
applyOperator _             _     = undefined

part1 :: String -> Int
part1 = versionSum . fst . parsePacket . toBinaryS

part2 :: String -> Int
part2 = evaluatePacket . fst . parsePacket . toBinaryS
