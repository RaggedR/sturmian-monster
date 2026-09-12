-- What does the player DO with the four bits?
--
-- Nothing.  He glues them into one symbol and indexes a table.  He never uses
-- the fact that there are four of them, that one is more recent than another,
-- how many strikes they contain, or anything else about them.
--
-- Proof: relabel the windows arbitrarily -- scramble the names, or reverse the
-- bits so "most recent" becomes "oldest" -- and he plays exactly the same.
module Main where

import qualified Data.Map.Strict as M
import Data.List (tails, nub)

n, len :: Int
n = 4
len = 20000

sturmian :: [Int]
sturmian = take len (head (dropWhile ((< len) . length) (iterate grow [0])))
  where grow = concatMap (\c -> if c == 0 then [0,1] else [0])

windowAt :: Int -> [Int]
windowAt k = take n (drop (k - n) sturmian)

allWindows :: [[Int]]
allWindows = nub [ take n t | t <- take 2000 (tails sturmian) ]

-- count what follows each window, then take the majority
tableBy :: Ord key => ([Int] -> key) -> M.Map key Char
tableBy key = M.map (\(z,o) -> if z >= o then 'A' else 'B') (M.fromListWith plus
  [ (key (take n t), if sturmian !! (i+n) == 0 then (1,0) else (0,1))
  | (i, t) <- zip [0..] (take (len - n) (tails sturmian)) ])
  where plus (a,b) (c,d) = (a+c, b+d)

play :: Ord key => ([Int] -> key) -> [Char]
play key = let t = tableBy key in [ t M.! key (windowAt k) | k <- [n .. n + 39] ]

------------------------------------------------------------------ three keys
asBits :: [Int] -> [Int]
asBits = id                                    -- the honest window

reversed :: [Int] -> [Int]
reversed = reverse                             -- oldest bit first: recency destroyed

scrambled :: [Int] -> Char
scrambled w = M.findWithDefault '?' w naming   -- arbitrary names, no structure at all
  where naming = M.fromList (zip allWindows "QZ7!m")

main :: IO ()
main = do
  putStrLn ("the 5 windows, and the arbitrary names I gave them: "
            ++ show (zip (map (map sym) allWindows) "QZ7!m"))
  putStrLn ""
  putStrLn "40 moves, played three ways (A = attack, B = block):"
  putStrLn ("  window as bits      : " ++ play asBits)
  putStrLn ("  window REVERSED     : " ++ play reversed)
  putStrLn ("  window as junk names: " ++ play scrambled)
  putStrLn ""
  putStrLn (if play asBits == play reversed && play asBits == play scrambled
            then "IDENTICAL.  The bits carry no information beyond WHICH of the 5 they are."
            else "they differ")
  where sym 0 = '.'
        sym _ = 'H'
