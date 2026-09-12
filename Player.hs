-- Each tick the monster throws one blow and you choose ONE move:
--
--   Attack -- you deal 1, and you take the blow (light 1, heavy 6)
--   Block  -- you take 0, and you deal 0
--
-- So a wrong guess costs you either way: block a light blow and you wasted
-- your turn; attack into a heavy and you eat 6.  The perfect player attacks
-- every light and blocks every heavy.
--
-- The blow sequence is a labelling of the one-loop graph (Coloured Graphs as
-- Directed Containers, sec. "Almost computable"): 0 = light, 1 = heavy.
module Main where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (foldl', tails)
import Text.Printf (printf)

data Move = Block | Attack deriving (Eq, Show)

damage :: Int -> Int
damage 0 = 1
damage _ = 6

-- Everyone is scored the same way.
score :: [Move] -> [Int] -> (Int, Int)          -- (dealt, taken)
score ms bs = foldl' step (0,0) (zip ms bs)
  where step (dealt, taken) (Attack, b) = (dealt + 1, taken + damage b)
        step (dealt, taken) (Block,  _) = (dealt,     taken)

------------------------------------------------------------- three monsters
len :: Int
len = 100000

periodic, sturmian, randomW :: [Int]
periodic = take len (cycle [0,1,1,0])
sturmian = take len (head (dropWhile ((< len) . length) (iterate grow [0])))
  where grow = concatMap (\c -> if c == 0 then [0,1] else [0])
randomW  = take len (map (\x -> fromIntegral ((x `div` 65536) `mod` 2)) (iterate lcg 42))
  where lcg x = (1103515245 * x + 12345) `mod` (2147483648 :: Integer)

-------------------------------------------------------------- the strategies
alwaysAttack, alwaysBlock :: [Int] -> [Move]
alwaysAttack w = map (const Attack) w
alwaysBlock  w = map (const Block)  w

-- Sees the blow that is landing NOW, decides for the NEXT one. One tick late.
blind :: [Int] -> [Move]
blind w = Attack : map (\b -> if b == 1 then Block else Attack) w

-- Sees the last n blows. Guesses the next from what that window did last time.
memory :: Int -> [Int] -> [Move]
memory n w = replicate n Attack ++ go M.empty (zip (windows n w) (drop n w))
  where go _ [] = []
        go m ((win, actual) : rest) =
          let guess = M.findWithDefault 0 win m
          in (if guess == 1 then Block else Attack) : go (M.insert win actual m) rest

-- Sees one blow ahead. This is the perfect player.
perfect :: [Int] -> [Move]
perfect w = map (\b -> if b == 1 then Block else Attack) w

------------------------------------------------- Robin's refinement, verbatim
classes :: [Int] -> Int -> Int
classes w n = S.size (S.fromList (windows (n+1) w))

windows :: Int -> [Int] -> [[Int]]
windows k w = [ take k t | t <- take (length w - k + 1) (tails w) ]

------------------------------------------------------------------------ main
main :: IO ()
main = do
  putStrLn "Sturmian monster, first 40 blows (H = heavy 6, . = light 1):"
  putStrLn ("  " ++ map (\c -> if c == 1 then 'H' else '.') (take 40 sturmian))
  putStrLn ""
  board "SturmiaN" sturmian
  board "periodic 0110" periodic
  board "random" randomW
  putStrLn "SPLITS -- memory-n windows that can still surprise you"
  putStrLn "  n | periodic | Sturmian |   random"
  mapM_ (\n -> printf "%3d | %8d | %8d | %8d\n" n
                 (classes periodic n - classes periodic (n-1))
                 (classes sturmian n - classes sturmian (n-1))
                 (classes randomW  n - classes randomW  (n-1))) [1..11::Int]

board :: String -> [Int] -> IO ()
board name w = do
  printf "=== %s ===\n" name
  putStrLn "strategy            | dealt | taken | taken per point dealt"
  mapM_ line ([ ("always attack", alwaysAttack), ("always block", alwaysBlock)
              , ("blind (last blow)", blind) ]
              ++ [ ("memory " ++ show n, memory n) | n <- [1,2,4,7,12,20] ]
              ++ [ ("perfect (1 ahead)", perfect) ])
  putStrLn ""
  where
    line (nm, strat) =
      let (d, t) = score (take 1000 (strat w)) (take 1000 w)
          ratio  = if d == 0 then "never wins" else printf "%.2f" (fromIntegral t / fromIntegral d :: Double)
      in printf "%-19s | %5d | %5d | %s\n" nm d t (ratio :: String)
