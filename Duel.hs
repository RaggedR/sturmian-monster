module Main where

import qualified Data.Map.Strict as M
import Text.Printf (printf)

-- 20 HP each.  Each tick the monster plays a bit from its tape, the player
-- plays a bit.  You must KILL the monster -- blocking never damages it.
--
--                  monster 0 (winds up)     monster 1 (strikes)
--   player 0  block    nothing                nothing (you guarded it)
--   player 1  attack   monster -2             player -2 (swung into it)
--
-- So: read him right and you deal 2.  Read him wrong on a strike and you eat 2.
-- Read him wrong on a wind-up and you merely wasted the turn.
-- Block forever and you survive forever, having achieved nothing.
payoff :: (Int, Int) -> (Int, Int)          -- -> (player loses, monster loses)
payoff (0, 0) = (0, 0)
payoff (0, 1) = (0, 0)
payoff (1, 0) = (0, 2)
payoff (1, 1) = (2, 0)
payoff _      = (0, 0)

startHP, timeLimit :: Int
startHP   = 20
timeLimit = 200

bestReply :: Int -> Int
bestReply m = if v 1 > v 0 then 1 else 0
  where v p = let (lose, deal) = payoff (p, m) in deal - lose

--------------------------------------------------------------------- monsters
len :: Int
len = 100000

periodic, sturmian, randomW :: [Int]
periodic = take len (cycle [0,1,1,0])
sturmian = take len (head (dropWhile ((< len) . length) (iterate grow [0])))
  where grow = concatMap (\c -> if c == 0 then [0,1] else [0])
randomW  = take len (map (\x -> fromIntegral ((x `div` 65536) `mod` 2)) (iterate lcg 42))
  where lcg x = (1103515245 * x + 12345) `mod` (2147483648 :: Integer)

------------------------------------------------------------------- the duel
type Strategy = [Int] -> Int                 -- sees the monster's PAST, picks a bit

duel :: Strategy -> [Int] -> (String, Int, Int, Int)
duel strat w = go startHP startHP [] (take timeLimit w) 0
  where
    go php mhp _ [] k         = ("stalemate", k, php, mhp)
    go php mhp hist (m:rest) k
      | mhp <= 0  = ("player wins", k, php, mhp)
      | php <= 0  = ("MONSTER wins", k, php, mhp)
      | otherwise = let (pl, ml) = payoff (strat hist, m)
                    in go (php - pl) (mhp - ml) (m : hist) rest (k+1)

------------------------------------------------------------------ strategies
always :: Int -> Strategy
always b _ = b

-- He has fought this monster before: a table learned from a stretch of the
-- tape he will not meet again in this fight.
learnTable :: Int -> [Int] -> M.Map [Int] Int
learnTable n w = M.map (\(z,o) -> if z >= o then 0 else 1) (M.fromListWith plus
                   [ (take n (drop i w), if w !! (i+n) == 0 then (1,0) else (0,1))
                   | i <- [5000 .. 9000] ])
  where plus (a,b) (c,d) = (a+c, b+d)

studied :: M.Map [Int] Int -> Int -> Strategy
studied tbl n hist
  | length hist < n = 0                                  -- ignorant: guard
  | otherwise       = bestReply (M.findWithDefault 1 (reverse (take n hist)) tbl)

--------------------------------------------------------------------------- ui
-- 400 separate fights, each starting at a different point in the monster's tape
fights :: [Int]
fights = [0, 7 .. 2800]

batch :: Strategy -> [Int] -> (Int, Double, Double)     -- (wins, mean ticks, mean HP left)
batch strat w = (length wins, avg (map tk wins), avg (map hp wins))
  where
    rs   = [ duel strat (drop o w) | o <- fights ]
    wins = [ r | r@(res,_,_,_) <- rs, res == "player wins" ]
    tk (_,k,_,_) = fromIntegral k
    hp (_,_,p,_) = fromIntegral p
    avg [] = 0 / 0
    avg xs = sum xs / fromIntegral (length xs)

main :: IO ()
main = do
  printf "best reply: monster 0 -> play %d,  monster 1 -> play %d   (they differ: reads matter)\n"
         (bestReply 0) (bestReply 1)
  printf "%d fights per strategy, each from a different point in the monster's tape.\n\n"
         (length fights)
  mapM_ board [("Sturmian", sturmian), ("periodic 0110", periodic), ("random", randomW)]
  putStrLn "ceiling: reading every tick correctly kills in 10 ticks at 20 HP."

board :: (String, [Int]) -> IO ()
board (nm, w) = do
  printf "=== %s ===\n" nm
  putStrLn "  strategy              win rate   mean ticks   mean HP left"
  row "always block" (always 0)
  row "always attack" (always 1)
  mapM_ (\n -> row ("studied, memory " ++ show n) (studied (learnTable n w) n)) [1,2,3,4,7,12]
  putStrLn ""
  where
    row lbl s = let (wn, mt, mh) = batch s w
                    pct = 100 * fromIntegral wn / fromIntegral (length fights) :: Double
                in printf "  %-20s %6.1f%%   %10s   %12s\n" lbl pct
                     (if wn == 0 then "-" else printf "%.1f" mt :: String)
                     (if wn == 0 then "-" else printf "%.1f" mh :: String)
