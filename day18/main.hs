module Main where

import Data.List (foldl1')


main :: IO ()
main = do
  contents <- getContents
  putStr "Part 1: "
  print (part1 $ lines contents)
  putStr "Part 2: "
  print (part2 $ lines contents)


data SnailfishNumber = Num Int | SFNum SnailfishNumber SnailfishNumber
fromNum :: SnailfishNumber -> Int
fromNum (Num n) = n
fromNum _ = undefined
both :: (SnailfishNumber -> a) -> SnailfishNumber -> (a,a)
both f (SFNum l r) = (f l,f r)
both _ _ = undefined

instance Show SnailfishNumber where
  show (SFNum l r) = "[" ++ show l ++ "," ++ show r ++ "]"
  show (Num i)     = show i

-- After a long trip down the garden path, easy as this, thanks to: https://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/stdclasses.html#sect8.3
readsSnailfishNumber :: ReadS SnailfishNumber
readsSnailfishNumber ('[':ls) = [(SFNum l r, xs) | (l, ',':rs) <- readsSnailfishNumber ls,
                                                   (r, ']':xs) <- readsSnailfishNumber rs]
readsSnailfishNumber ns       = [(Num n, rest) | (n, rest) <- reads ns]

-- Missing info from 404 link in reference above is hard to find but turns out to be just this:
instance Read SnailfishNumber where
  readsPrec d = readParen (d > 10) readsSnailfishNumber

-- (+)  is reserved for Num types, which is a hill too steep to climb. See https://stackoverflow.com/a/8331010/3697870
-- (++) is reserved for Monoid types, but we're a concrete type without a type variable,
--      so we can't be a Functor, hence defined `both` instead of `fmap`, so that's out.
-- (+++) chosen as better than `add`, due to its automatic infix-ness
(+++) :: SnailfishNumber -> SnailfishNumber -> SnailfishNumber
a +++ b = SFNum a b


explode :: SnailfishNumber -> Maybe SnailfishNumber
explode = maybe Nothing (\(n,_,_) -> Just n) . explode' 0

data Most = Leftmost | Rightmost
add :: Most -> Int -> SnailfishNumber -> SnailfishNumber
add Leftmost  n (SFNum l r)   = SFNum (add Leftmost n l) r
add Rightmost n (SFNum l r)   = SFNum l (add Rightmost n r)
add _         n (Num m)       = Num $ n + m

explode' :: Int -> SnailfishNumber -> Maybe (SnailfishNumber, Int, Int)
explode' _ (Num n)          = Nothing
explode' 4 (SFNum l r)      = Just (Num 0, fromNum l, fromNum r)
explode' n (SFNum l r)      = let (l',r') = both (explode' (succ n)) $ SFNum l r in
                              maybe (explodeRight =<< r') explodeLeft l' where
  explodeLeft  (l, ln, rn)  = Just (SFNum l  (add Leftmost rn r), ln, 0)
  explodeRight (r, ln, rn)  = Just (SFNum (add Rightmost ln l) r, 0, rn)


split :: SnailfishNumber -> Maybe SnailfishNumber
split (Num n)
  | n >= 10           = Just $ SFNum (Num $ n `div` 2) (Num $ (n+1) `div` 2)
  | otherwise         = Nothing
split (SFNum l r)     = let (l',r') = both split $ SFNum l r in
                        maybe (splitRight =<< r') splitLeft l' where
  splitLeft l'        = Just $ SFNum l' r
  splitRight r'       = Just $ SFNum l r'


reduce :: SnailfishNumber -> SnailfishNumber
reduce n    = maybe (reduce' n) reduce $ explode n where
  reduce' n = maybe n           reduce $ split n


magnitude :: SnailfishNumber -> Int
magnitude (Num n)     = n
magnitude (SFNum l r) = (3 * magnitude l) + (2 * magnitude r)


part1 :: [String] -> Int
part1 = magnitude . foldl1' (\l r -> reduce $ l +++ r) . map read

part2 :: [String] -> Int
part2 inp = let ns = map read inp in
            maximum [magnitude . reduce $ l +++ r | l <- ns, r <- ns]

