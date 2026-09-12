-- Can the player throw away his memory and become a pure lookup?
--
-- A "table" maps a window of the last n blows to the blow that follows it.
-- That is only a FUNCTION if every window has exactly one follower.
-- If some window is followed sometimes by light and sometimes by heavy,
-- there is no table, and the player is stuck carrying state forever.
module Main where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (tails, intercalate)
import Text.Printf (printf)

------------------------------------------------------------------ the tape
data Zipper a = Zipper [a] a [a]          -- past (nearest first) | here | future

extract :: Zipper a -> a
extract (Zipper _ a _) = a

left, right :: Zipper a -> Zipper a
left  (Zipper (l:ls) a rs) = Zipper ls l (a:rs)
right (Zipper ls a (r:rs)) = Zipper (a:ls) r rs

instance Functor Zipper where
  fmap f (Zipper ls a rs) = Zipper (map f ls) (f a) (map f rs)

duplicate :: Zipper a -> Zipper (Zipper a)
duplicate z = Zipper (tail (iterate left z)) z (tail (iterate right z))

extend :: (Zipper a -> b) -> Zipper a -> Zipper b
extend f = fmap f . duplicate

walk :: Int -> Zipper a -> [a]
walk 0 _ = []
walk n z = extract z : walk (n-1) (right z)

--------------------------------------------------------------- the monsters
len :: Int
len = 100000

periodic, sturmian, randomW :: [Int]
periodic = take len (cycle [0,1,1,0])
sturmian = take len (head (dropWhile ((< len) . length) (iterate grow [0])))
  where grow = concatMap (\c -> if c == 0 then [0,1] else [0])
randomW  = take len (map (\x -> fromIntegral ((x `div` 65536) `mod` 2)) (iterate lcg 42))
  where lcg x = (1103515245 * x + 12345) `mod` (2147483648 :: Integer)

windows :: Int -> [Int] -> [[Int]]
windows k w = [ take k t | t <- take (length w - k + 1) (tails w) ]

------------------------------------------------- what follows each window?
followers :: Int -> [Int] -> M.Map [Int] (S.Set Int)
followers n w = M.fromListWith S.union
  [ (win, S.singleton nxt) | (win, nxt) <- zip (windows n w) (drop n w) ]

ambiguous :: Int -> [Int] -> [[Int]]                   -- windows with TWO followers
ambiguous n w = M.keys (M.filter ((> 1) . S.size) (followers n w))

table :: Int -> [Int] -> M.Map [Int] Int               -- the lookup, if there is one
table n w = M.map (head . S.toList) (followers n w)

------------------------------------ THE COKLEISLI PLAYER.  No state at all.
-- Stand on a blow. Look n squares LEFT. Look the window up. That is the move.
data Move = Block | Attack deriving (Eq, Show)

player :: Int -> M.Map [Int] Int -> Zipper Int -> Move
player n tbl z =
  let win = reverse [ extract (iterate left z !! i) | i <- [1..n] ]
  in if M.findWithDefault 0 win tbl == 1 then Block else Attack

tapeOf :: [Int] -> Zipper Int
tapeOf (b:bs) = Zipper (repeat 0) b bs

score :: [Move] -> [Int] -> (Int, Int)
score ms bs = foldr acc (0,0) (zip ms bs)
  where acc (Attack, b) (d,t) = (d+1, t + if b == 1 then 6 else 1)
        acc (Block,  _) (d,t) = (d, t)

------------------------------------------------------------------------ main
main :: IO ()
main = do
  putStrLn "HOW MANY WINDOWS HAVE TWO DIFFERENT FOLLOWERS?"
  putStrLn "(a window with two followers = no table exists = state cannot be discarded)"
  putStrLn "  n | periodic | Sturmian | random"
  mapM_ (\n -> printf "%3d | %8d | %8d | %6d\n" n
                 (length (ambiguous n periodic))
                 (length (ambiguous n sturmian))
                 (length (ambiguous n randomW))) [1..8::Int]
  putStrLn ""
  putStrLn "THE ACTUAL AMBIGUOUS WINDOWS (Sturmian) -- this is the hidden state:"
  mapM_ (\n -> printf "  n=%d : %s\n" n (show (ambiguous n sturmian))) [1..6::Int]
  putStrLn ""
  putStrLn "  periodic, n=2 : " >> print (ambiguous 2 periodic)
  putStrLn "  ^ empty. every window has ONE follower. the table is a function."
  putStrLn ""
  putStrLn "NOW PLAY, with no state -- just  extend (player n tbl)  over the tape:"
  mapM_ go [("periodic", periodic), ("Sturmian", sturmian), ("random", randomW)]
  where
    n = 8
    go (nm, w) = do
      let moves    = drop n (walk 1000 (extend (player n (table n w)) (tapeOf w)))
          (d, t)   = score moves (drop n (take 1000 w))   -- skip the warm-up
      printf "  %-9s dealt %4d  taken %4d  ->  %.2f taken per point dealt%s\n"
             nm d t (fromIntegral t / fromIntegral d :: Double)
             (if null (ambiguous n w) then "   (table is a function: PERFECT)" else "")
