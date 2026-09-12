-- Is the confident player still a coKleisli map?
--
-- Confident.hs computes him with a tail-recursive loop carrying a Map.
-- That LOOKS like state, so it looks like the comonad is gone.
--
-- Here he is written the other way: a plain function  Past Int -> Move  that
-- recomputes everything from the history it can see, with no state at all,
-- dragged over the tape with  extend.  If the two agree, the loop was never
-- state -- it was this function with the recomputation cached.
module Main where

import qualified Data.Map.Strict as M
import Text.Printf (printf)

------------------------------------------------------------- the past comonad
data Past a = Past a [a]

extract :: Past a -> a
extract (Past a _) = a

instance Functor Past where
  fmap f (Past a hs) = Past (f a) (map f hs)

duplicate :: Past a -> Past (Past a)
duplicate p@(Past _ hs) = Past p (go hs)
  where go []       = []
        go (x : xs) = Past x xs : go xs

extend :: (Past a -> b) -> Past a -> Past b
extend f = fmap f . duplicate

toList :: Past a -> [a]
toList (Past a hs) = a : hs

worldAt :: Int -> [Int] -> Past Int
worldAt k w = Past (w !! (k-1)) (reverse (take (k-1) w))

------------------------------------------------------------------ the monster
horizon, len :: Int
horizon = 400
len     = 100000

periodic, sturmian, randomW :: [Int]
periodic = take len (cycle [0,1,1,0])
sturmian = take len (head (dropWhile ((< len) . length) (iterate grow [0])))
  where grow = concatMap (\c -> if c == 0 then [0,1] else [0])
randomW  = take len (map (\x -> fromIntegral ((x `div` 65536) `mod` 2)) (iterate lcg 42))
  where lcg x = (1103515245 * x + 12345) `mod` (2147483648 :: Integer)

cost :: Int -> Int
cost 0 = 1
cost _ = 6

------------------------------------------- (A) the loop, exactly as Confident.hs
loopScore :: Int -> [Int] -> Int
loopScore n w = go M.empty [] (take horizon w) 0
  where
    go _ _ [] acc = acc
    go tbl hist (blow : rest) acc =
      let haveWin = length hist >= n
          win     = reverse (take n hist)
          (l, h)  = M.findWithDefault (0,0) win tbl
          attacking = haveWin && 2*l - 3*(h+1) > 0
          acc'    = if attacking then acc + 3 - cost blow else acc
          tbl'    = if haveWin
                      then M.insertWith (\_ (a,b) -> if blow == 0 then (a+1,b) else (a,b+1))
                                        win (if blow == 0 then (1,0) else (0,1)) tbl
                      else tbl
      in go tbl' (blow : hist) rest acc'

------------------------------- (B) the coKleisli map.  No state.  Just looking.
data Move = Block | Attack deriving Eq

player :: Int -> Past Int -> Move
player n p =
  let chron = reverse (toList p)                      -- everything he has seen
      k     = length chron
      counts = M.fromListWith plus
                 [ (take n (drop i chron), one (chron !! (i+n)))
                 | i <- [0 .. k - n - 1] ]
      win   = drop (k - n) chron                      -- the last n blows
      (l,h) = M.findWithDefault (0,0) win counts
  in if k >= n && 2*l - 3*(h+1) > 0 then Attack else Block
  where
    one 0 = (1,0)
    one _ = (0,1)
    plus (a,b) (c,d) = (a+c, b+d)

extendScore :: Int -> [Int] -> Int
extendScore n w =
  sum [ if m == Attack then 3 - cost b else 0
      | (m, b) <- zip moves (drop 1 (take (horizon+1) w)) ]
  where moves = reverse (toList (extend (player n) (worldAt (horizon - 1) w)))

--------------------------------------------------------------------------- main
main :: IO ()
main = do
  printf "%-16s %4s | %10s | %10s | %s\n" "monster" "n" "loop (A)" "extend (B)" "same?"
  sequence_ [ printf "%-16s %4d | %10d | %10d | %s\n" nm n a b
                (if a == b then "yes" else "NO")
            | (nm, w) <- [("Sturmian", sturmian), ("periodic 0110", periodic), ("random", randomW)]
            , n <- [1,2,4,7]
            , let a = loopScore n w
            , let b = extendScore n w ]
