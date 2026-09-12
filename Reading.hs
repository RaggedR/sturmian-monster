-- How a strategy actually reads the tape.
--
-- Every strategy in this project is built from ONE primitive:
--
--      peek k  =  extract . back k        -- "the blow k ticks before now"
--
-- That is the whole vocabulary.  Strategies differ only in WHICH k they use.
-- No strategy ever calls duplicate.  duplicate is called once, by extend,
-- to stand the strategy at every tick in turn.
module Main where

import qualified Data.Map.Strict as M
import Data.List (intercalate)

data Past a = Past a [a]                  -- now, then older

extract :: Past a -> a
extract (Past a _) = a

back :: Int -> Past a -> Past a           -- step k ticks into the past
back 0 p = p
back k (Past _ (h:hs)) = back (k-1) (Past h hs)
back _ p = p

peek :: Int -> Past Int -> Int            -- THE primitive: extract . back k
peek k = extract . back k

instance Functor Past where
  fmap f (Past a hs) = Past (f a) (map f hs)

duplicate :: Past a -> Past (Past a)
duplicate p@(Past _ hs) = Past p (go hs)
  where go []       = []
        go (x : xs) = Past x xs : go xs

extend :: (Past a -> b) -> Past a -> Past b
extend f = fmap f . duplicate           -- <-- the ONLY use of duplicate anywhere

toList :: Past a -> [a]
toList (Past a hs) = a : hs

----------------------------------------------------------------- strategies
data Move = Block | Attack deriving (Eq, Show)

alwaysAttack :: Past Int -> Move
alwaysAttack _ = Attack                              -- reads nothing

lastBlow :: Past Int -> Move
lastBlow p = if peek 0 p == 1 then Attack else Block -- reads peek 0

memory :: Int -> M.Map [Int] Int -> Past Int -> Move
memory n tbl p =                                     -- reads peek (n-1) .. peek 0
  let win = [ peek k p | k <- [n-1, n-2 .. 0] ]
  in if M.findWithDefault 1 win tbl == 1 then Block else Attack

-------------------------------------------------------------------- monster
sturmian :: [Int]
sturmian = take 400 (head (dropWhile ((< 400) . length) (iterate grow [0])))
  where grow = concatMap (\c -> if c == 0 then [0,1] else [0])

tbl2 :: M.Map [Int] Int
tbl2 = M.fromList [([0,0],1), ([0,1],0), ([1,0],0)]   -- learned earlier

worldAt :: Int -> Past Int
worldAt k = Past (sturmian !! k) (reverse (take k sturmian))

sym :: Int -> Char
sym 0 = '.'
sym _ = 'H'

------------------------------------------------------------------------ main
main :: IO ()
main = do
  putStrLn ("tape:   " ++ map sym (take 12 sturmian) ++ "   (H = strike)")
  putStrLn  "tick:   0123456789.."
  putStrLn ""
  putStrLn "AT TICK 8, what the primitives return:"
  let p = worldAt 8
  mapM_ (\k -> putStrLn ("   peek " ++ show k ++ " p  =  " ++ [sym (peek k p)]
                         ++ "      (extract . back " ++ show k ++ ")")) [0..3]
  putStrLn ""
  putStrLn "WHICH PRIMITIVES EACH STRATEGY CALLS:"
  putStrLn "   always attack   : none                        -> constant"
  putStrLn "   last blow only  : peek 0                      -> 1 extract"
  putStrLn "   memory 2        : peek 1, peek 0              -> 2 extracts"
  putStrLn "   memory 4        : peek 3, peek 2, peek 1, peek 0 -> 4 extracts"
  putStrLn "   (no strategy calls duplicate)"
  putStrLn ""
  putStrLn "NOW extend DRAGS memory-2 ALONG THE TAPE."
  putStrLn "extend supplies the position; the strategy supplies the reading."
  putStrLn ""
  putStrLn "  tick | peek1 peek0 | window | move"
  mapM_ trace [2 .. 11]
  putStrLn ""
  let moves = reverse (toList (extend (memory 2 tbl2) (worldAt 11)))
  putStrLn ("  extend (memory 2 tbl) (worldAt 11)  =  "
            ++ intercalate " " (map (take 1 . show) moves))
  where
    trace k =
      let p = worldAt k
          w = [peek 1 p, peek 0 p]
      in putStrLn ("  " ++ pad k ++ "   |   " ++ [sym (peek 1 p)] ++ "     "
                   ++ [sym (peek 0 p)] ++ "   |  " ++ map sym w
                   ++ "    | " ++ show (memory 2 tbl2 p))
    pad k = let t = show k in replicate (2 - length t) ' ' ++ t
