module Main where

import Data.Char (toUpper, isDigit, digitToInt)
import Data.List (nub, elemIndex, find)
import Data.Maybe (isJust, fromJust, isNothing, mapMaybe, listToMaybe)

import Debug.Trace (trace)
import Text.Read (readMaybe)

--import Control.Parallel.Strategies (parMap, rdeepseq)


main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)

data Command = INP | ADD | MUL | DIV | MOD | EQL deriving (Show, Read, Eq)
data Variable = W | X | Y | Z deriving (Show, Read)

--data Instruction = Instruction { cmd :: Command, arg1 :: Variable, arg2 :: Maybe Either Variable Integer }
--data Instruction = SingleArg Command Variable |
--                   DoubleArg Command Variable (Either Variable Int) deriving (Show)
data Instruction = Instruction Command Variable (Maybe (Either Variable Int)) deriving (Show)
type Program = [Instruction]
type Input = [Int] -- arbitrary number of ints that will be provided to INP in turn
data State = State Int Int Int Int deriving (Show) -- value of W, X, Y and Z respectively


makeProgram :: [String] -> Program
makeProgram = map makeInstruction

makeInstruction :: String -> Instruction
makeInstruction line = case words $ map toUpper line of
  [cmd, arg1]       -> instructionPartial cmd arg1 Nothing
  [cmd, arg1, arg2] -> instructionPartial cmd arg1 (Just $ readAsEitherVarOrInt arg2)
  _                 -> error "Bad input."
  where instructionPartial cmd arg1 = Instruction (readAsCmd cmd) (readAsVar arg1)

readAsCmd :: String -> Command
readAsCmd s = case readMaybe s of
  Just c  -> c
  Nothing -> error "Invalid command."

readAsVar :: String -> Variable
readAsVar s = case readMaybe s of
  Just v  -> v
  Nothing -> error "Invalid variable."

readAsEitherVarOrInt :: String -> Either Variable Int
readAsEitherVarOrInt s = case readMaybe s :: Maybe Variable of
  Just v  -> Left v
  Nothing -> case readMaybe s :: Maybe Int of
    Just i  -> Right i
    Nothing -> error "Invalid variable or number."

makeInput :: String -> Input
makeInput = map digitToInt


runALU :: Program -> Input -> State
runALU = doALU (State 0 0 0 0) where
  doALU :: State -> Program -> Input -> State
  doALU state (cmd:cmds) inp = let (newState, newInp) = stepALU state cmd inp
                               in doALU newState cmds newInp
  doALU state [] _ = state

stepALU :: State -> Instruction -> Input -> (State, Input)
stepALU state (Instruction ins a b) inp = if ins == INP then (decodeI state a (head inp), tail inp)
                                                        else (decode1 state ins a b, inp) where
  decodeI (State w x y z) W inp             = State inp x y z
  decodeI (State w x y z) X inp             = State w inp y z
  decodeI (State w x y z) Y inp             = State w x inp z
  decodeI (State w x y z) Z inp             = State w x y inp
  decode1 (State w x y z) ins W b           = State (decode2 state ins w (fromJust b)) x y z
  decode1 (State w x y z) ins X b           = State w (decode2 state ins x (fromJust b)) y z
  decode1 (State w x y z) ins Y b           = State w x (decode2 state ins y (fromJust b)) z
  decode1 (State w x y z) ins Z b           = State w x y (decode2 state ins z (fromJust b))
  decode2 (State w x y z) ins a (Left  W)   = decode3 ins a w
  decode2 (State w x y z) ins a (Left  X)   = decode3 ins a x
  decode2 (State w x y z) ins a (Left  Y)   = decode3 ins a y
  decode2 (State w x y z) ins a (Left  Z)   = decode3 ins a z
  decode2 (State w x y z) ins a (Right b)   = decode3 ins a b
  decode3 ADD a b           = a+b
  decode3 MUL a b           = a*b
  decode3 DIV a b           = a `div` b
  decode3 MOD a b           = a `mod` b
  decode3 EQL a b           = if a == b then 1 else 0
  decode3 INP _ _           = error "Not possible."


allModelNumbers :: [String]
allModelNumbers = let dig = reverse ['1'..'9'] in [modelNum | modelNum <- sequence (replicate 14 dig)]


part1 :: [String] -> (String, State)
--part1 inp = let i = [9,9,9,9,9,9,9,9,9,9,9,9,9,9] in
--            (i, runALU (makeProgram inp) i)
part1 inp = let prog = makeProgram inp
--                inps = map makeInput allModelNumbers
--                outs = map (\i -> (i, runALU prog i)) inps
--                modNum = filter (notElem '0') (map show [99999999999999,99999999999998..11111111111111])
                modNum = sequence $ replicate 14 ['9', '8'..'1']
                outs = map (\i -> (i, runALU prog (makeInput i))) modNum
                good = dropWhile (not . goodOutput) outs
            in head good
  --where goodOutput (_,State _ _ _ z) = z == 0
  where goodOutput (i,State w x y z) = if drop 8 i == "111111" then trace (show (i,State w x y z)) $ z == 0
                                                               else z == 0


part2 :: [String] -> Int
part2 = undefined
