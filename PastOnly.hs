-- The player sees only what the monster has already done.
--
-- His world is  Past a = Past a [a]  -- the blow that just landed, then older ones.
-- There is no  forward  function.  There is nothing in the type that points at
-- the future.  A clairvoyant player is not merely discouraged, it cannot be
-- written: there is nowhere to look.
module Main where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (tails)
import Text.Printf (printf)

--------------------------------------------------------------- the past only
data Past a = Past a [a]            -- now (most recent blow), then older

extract :: Past a -> a              -- the blow that just landed
extract (Past a _) = a

instance Functor Past where
  fmap f (Past a hs) = Past (f a) (map f hs)

duplicate :: Past a -> Past (Past a)          -- every earlier moment, with its own history
duplicate p@(Past _ hs) = Past p (go hs)
  where go []       = []
        go (x : xs) = Past x xs : go xs

extend :: (Past a -> b) -> Past a -> Past b
extend f = fmap f . duplicate

toList :: Past a -> [a]
toList (Past a hs) = a : hs

-- the world at tick k: he has seen blows 0..k-1 and is about to meet blow k
worldAt :: Int -> [Int] -> Past Int
worldAt k w = Past (w !! (k-1)) (reverse (take (k-1) w))

--------------------------------------------------------------- the monsters
len :: Int
len = 100000

periodic, sturmian, randomW :: [Int]
periodic = take len (cycle [0,1,1,0])
sturmian = take len (head (dropWhile ((< len) . length) (iterate grow [0])))
  where grow = concatMap (\c -> if c == 0 then [0,1] else [0])
randomW  = take len (map (\x -> fromIntegral ((x `div` 65536) `mod` 2)) (iterate lcg 42))
  where lcg x = (1103515245 * x + 12345) `mod` (2147483648 :: Integer)

--------------------------------------------------------------- the strategies
data Move = Block | Attack deriving (Eq, Show)

alwaysAttack, alwaysBlock, lastBlow :: Past Int -> Move
alwaysAttack _ = Attack
alwaysBlock  _ = Block
lastBlow p = if extract p == 1 then Block else Attack

-- Look back n blows, consult a fixed table. No state. Pure coKleisli map.
memory :: Int -> M.Map [Int] Int -> Past Int -> Move
memory n tbl p =
  let win = reverse (take n (toList p))        -- the last n blows, oldest first
  in if M.findWithDefault 0 win tbl == 1 then Block else Attack

-- the table, learned once from the monster's whole history
table :: Int -> [Int] -> M.Map [Int] Int
table n w = M.map (head . S.toList) (M.fromListWith S.union
  [ (win, S.singleton nxt)
  | (win, nxt) <- zip [ take n t | t <- take (length w - n) (tails w) ] (drop n w) ])

ambiguous :: Int -> [Int] -> Int
ambiguous n w = length (filter ((> 1) . S.size) (M.elems (M.fromListWith S.union
  [ (win, S.singleton nxt)
  | (win, nxt) <- zip [ take n t | t <- take (length w - n) (tails w) ] (drop n w) ])))

--------------------------------------------------------------------- scoring
ticks :: Int
ticks = 1000

runP :: (Past Int -> Move) -> [Int] -> (Int, Int)
runP f w = foldr acc (0,0) (zip moves (drop 25 (take (ticks+1) w)))
  where moves = drop 24 (reverse (toList (extend f (worldAt ticks w))))
        acc (Attack, b) (d,t) = (d+1, t + if b == 1 then 6 else 1)
        acc (Block,  _) (d,t) = (d, t)

main :: IO ()
main = mapM_ board [("Sturmian", sturmian), ("periodic 0110", periodic), ("random", randomW)]

board :: (String, [Int]) -> IO ()
board (nm, w) = do
  printf "=== %s ===\n" nm
  putStrLn "strategy          | blind spots | dealt | taken | taken per point dealt"
  row "always attack"  Nothing  alwaysAttack
  row "always block"   Nothing  alwaysBlock
  row "last blow only" (Just 1) lastBlow
  mapM_ (\n -> row ("memory " ++ show n) (Just n) (memory n (table n (drop (ticks + 64) w)))) [2,4,7,12,20]
  putStrLn ""
  where
    row nm' mn f =
      let (d, t) = runP f w
          amb    = maybe "     -" (printf "%6d" . (`ambiguous` w)) mn :: String
          ratio  = if d == 0 then "never wins" else printf "%.2f" (fromIntegral t / fromIntegral d :: Double)
      in printf "%-17s | %s      | %5d | %5d | %s\n" nm' amb d t (ratio :: String)
