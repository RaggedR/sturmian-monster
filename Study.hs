-- STUDY THEN FIGHT.
--
-- Phase 1 (t < studyFor): block every tick.  You deal nothing, you take
--   nothing, and you watch.  This is buying safety while you are ignorant.
-- Phase 2: consult what you learned.  Attack if you expect a light blow.
--   An unseen window means you are still ignorant there -- so block.
--
-- You keep learning in phase 2 as well: you always see a blow after it lands.
-- So studying does not buy information, it buys NOT DYING while you get it.
--
-- Attack deals 3.  Light blow costs 1, heavy costs 6.  Blocking costs nothing
-- but the turn.  So: attack into a light = +2, attack into a heavy = -3.
module Main where

import qualified Data.Map.Strict as M
import Data.List (tails, maximumBy)
import Data.Ord (comparing)
import Text.Printf (printf)

horizon :: Int
horizon = 2000

len :: Int
len = 100000

periodic, sturmian, randomW :: [Int]
periodic = take len (cycle [0,1,1,0])
sturmian = take len (head (dropWhile ((< len) . length) (iterate grow [0])))
  where grow = concatMap (\c -> if c == 0 then [0,1] else [0])
randomW  = take len (map (\x -> fromIntegral ((x `div` 65536) `mod` 2)) (iterate lcg 42))
  where lcg x = (1103515245 * x + 12345) `mod` (2147483648 :: Integer)

cost :: Int -> Int
cost 0 = 1
cost _ = 6

-- run one (memory n, study for t) player over the horizon; return (dealt, taken)
run :: Int -> Int -> [Int] -> (Int, Int)
run n studyFor w = go M.empty [] 0 (take horizon w) 0 0
  where
    go _ _ _ [] d t = (d, t)
    go tbl hist k (blow : rest) d t =
      let win      = reverse (take n hist)
          known    = length hist >= n
          attacking = k >= studyFor && known && M.lookup win tbl == Just 0
          (d', t') = if attacking then (d + 3, t + cost blow) else (d, t)
          tbl'     = if known then M.insert win blow tbl else tbl
      in go tbl' (blow : hist) (k+1) rest d' t'

net :: Int -> Int -> [Int] -> Int
net n s w = let (d, t) = run n s w in d - t

--------------------------------------------------------------------------- ui
studies :: [Int]
studies = [0, 25, 50, 100, 200, 400, 800]

depths :: [Int]
depths = [1..12]

grid :: (String, [Int]) -> IO ()
grid (nm, w) = do
  printf "=== %s ===   net score (damage dealt - damage taken) over %d ticks\n" nm horizon
  putStrLn ("  n \\ study " ++ concatMap (printf "%7d") studies)
  mapM_ rowFor depths
  let best = maximumBy (comparing (\(_,_,v) -> v))
               [ (n, s, net n s w) | n <- depths, s <- studies ]
  let (bn, bs, bv) = best
  printf "  best: memory %d, study %d ticks  ->  net %d\n\n" bn bs bv
  where
    rowFor n = putStrLn (printf "%5d     " n ++ concatMap (\s -> printf "%7d" (net n s w)) studies)

main :: IO ()
main = mapM_ grid [("Sturmian", sturmian), ("periodic 0110", periodic), ("random", randomW)]
