-- What happens to the four bits.
--
--   1. read them:      the last 4 blows, e.g.  [0,0,1,0]
--   2. look them up:   a table says what usually follows that window
--   3. best reply:     expect a wind-up -> ATTACK, expect a strike -> BLOCK
--
-- The table was built by counting, over a stretch of tape the player has seen
-- before, how often each window was followed by each thing.
module Main where

import qualified Data.Map.Strict as M
import Data.List (tails)
import Text.Printf (printf)

memoryDepth, len :: Int
memoryDepth = 4
len = 100000

sturmian :: [Int]
sturmian = take len (head (dropWhile ((< len) . length) (iterate grow [0])))
  where grow = concatMap (\c -> if c == 0 then [0,1] else [0])

sym :: Int -> Char
sym 0 = '.'
sym _ = 'H'

-- step 1+2: count what follows each window
counts :: M.Map [Int] (Int, Int)          -- window -> (windups after, strikes after)
counts = M.fromListWith plus
  [ (take memoryDepth t, if sturmian !! (i + memoryDepth) == 0 then (1,0) else (0,1))
  | (i, t) <- zip [0 ..] (take (len - memoryDepth) (tails sturmian)) ]
  where plus (a,b) (c,d) = (a+c, b+d)

-- step 3: the finished player, as a table from window to move
player :: M.Map [Int] String
player = M.map (\(z,o) -> if z >= o then "ATTACK" else "BLOCK ") counts

windowAt :: Int -> [Int]
windowAt k = take memoryDepth (drop (k - memoryDepth) sturmian)

main :: IO ()
main = do
  putStrLn "THE ENTIRE MEMORY-4 PLAYER.  This is all of him.\n"
  putStrLn "  window | followed by windup | by strike | he expects | he plays"
  mapM_ line (M.toList counts)
  printf "\n  Only %d windows of length 4 ever occur in this monster, so the player\n"
         (M.size counts)
  putStrLn "  has exactly that many states.  The 4 bits are the NAME of the state."
  putStrLn ""
  putStrLn "ONE TICK, END TO END (tick 9):"
  let w = windowAt 9
  printf "   1. read      peek 3,2,1,0  ->  %s\n" (map sym w)
  printf "   2. look up   counts ! %s   ->  %s\n" (map sym w) (show (counts M.! w))
  printf "   3. best reply                ->  %s\n" (player M.! w)
  putStrLn ""
  putStrLn "THE SAME THREE STEPS AT EVERY TICK:"
  putStrLn "  tick | window | plays"
  mapM_ (\k -> printf "  %4d |  %s   | %s\n" k (map sym (windowAt k)) (player M.! windowAt k))
        [4 .. 15]
  where
    line (w, (z, o)) =
      printf "  %s   | %18d | %9d | %-10s | %s\n" (map sym w) z o
             (if z >= o then "wind-up" else "strike")
             ((if z > 0 && o > 0 then "* " else "  ") ++ player M.! w)
