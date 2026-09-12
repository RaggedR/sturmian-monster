-- CHIP SCORING -- the clock replaced by attrition.
--
-- Robin's proposal: blocking a strike is no longer free, it costs 1.  Swinging
-- into a strike still costs 5.  Swinging into an open beat still deals 2.
-- There is NO time limit: the fight ends when somebody runs out of HP.
--
--            monster OPEN        monster STRIKE
--   BLOCK      nothing             -1   (chip)
--   ATTACK     2 damage            -5   (whiff)
--
-- Stalling is now paid for in the same currency as everything else, so the
-- exchange rate of difficulty-level.md ("time converts into certainty") stops
-- being a metaphor and becomes an actual subtraction.
--
-- For 1/2 < alpha < 2/3 the word contains no ".." and no "HHH", so of the three
-- length-2 contexts exactly one is undetermined:
--
--   H.  -> H   certain strike   never swing   (-5 if you do)
--   HH  -> .   certain open     always swing  (+2, free)
--   .H  -> ?   THE SPLIT
--
-- and the sequence of outcomes at the ".H" sites is itself Sturmian.  So the
-- game renormalises, and a player who recurses should beat one that memorises.
module Main where

import qualified Data.Map.Strict as M
import Text.Printf (printf)

chipD, hitD, whiffD, capBeats :: Int
chipD = 1; hitD = 2; whiffD = 5; capBeats = 5000

fights :: Int
fights = 300

sturm :: Double -> Double -> [Int]
sturm a rho =
  [ floor (fromIntegral (k+1)*a + rho) - floor (fromIntegral k*a + rho) | k <- [0::Int ..] ]

lcg :: Integer -> Integer
lcg x = (1103515245*x + 12345) `mod` 2147483648
unit :: Integer -> Double
unit r = fromIntegral (r `mod` 1000000) / 1000000

-- a player sees the beats so far, most recent FIRST
type Player = [Int] -> Bool

play :: Int -> Int -> [Int] -> Player -> Bool
play p0 m0 tape decide = go p0 m0 [] tape capBeats
  where
    go _   mhp _ _ _ | mhp <= 0 = True
    go php _   _ _ _ | php <= 0 = False
    go _   _   _ _ 0            = False
    go php mhp hist (b:rest) n
      | decide hist = if b == 0 then go php (mhp - hitD)   (b:hist) rest (n-1)
                                else go (php - whiffD) mhp (b:hist) rest (n-1)
      | otherwise   = if b == 1 then go (php - chipD) mhp  (b:hist) rest (n-1)
                                else go php mhp            (b:hist) rest (n-1)
    go _ _ _ [] _ = False

--------------------------------------------------------------------- players
mash, turtle, certain, afterH :: Player
mash    _ = True
turtle  _ = False
certain h = take 2 h == [1,1]        -- only the guaranteed opening, after HH
afterH  h = take 1 h == [1]          -- swing at the split too, blind

-- what follows each occurrence of the ambiguous context ".H".
-- hist is most-recent-first, so ".H" reads as [1,0] looking backwards.
-- forward order: the symbol following each ".H"
deriveFwd :: [Int] -> [Int]
deriveFwd w = [ c | (a,b,c) <- zip3 w (drop 1 w) (drop 2 w), a == 0, b == 1 ]

-- same thing read backwards out of a most-recent-first history, and lazy, so
-- asking for the last k derived symbols only walks back as far as it must
deriveBack :: [Int] -> [Int]
deriveBack h = [ c | (c,b,a) <- zip3 h (drop 1 h) (drop 2 h), a == 0, b == 1 ]

-- majority-vote next-symbol table over windows of length k
table :: Int -> [Int] -> M.Map [Int] Int
table k s = M.map (\(z,o) -> if z >= o then 0 else 1) (M.fromListWith plus
  [ (w, if nx == 0 then (1,0) else (0,1))
  | (w, nx) <- zip (map (take k) (iterate tail s)) (drop k s) ])
  where plus (a,b) (c,d) = (a+c, b+d)

-- memory-k on the raw beat history
raw :: Int -> [Int] -> Player
raw k train = \h -> let w = take k h
                    in length w == k && M.findWithDefault 1 (reverse w) tb == 0
  where tb = table k train

-- the two certainties, and a memory-k table on the DERIVED word at the split
renorm :: Int -> [Int] -> Player
renorm k train = go
  where
    tb = table k (deriveFwd train)
    go h = case h of
      (1:0:_) -> let dh = take k (deriveBack h)          -- at the split: recurse
                 in length dh == k
                    && M.findWithDefault 1 (reverse dh) tb == 0
      (1:1:_) -> True                                    -- after HH: certain open
      (0:_)   -> False                                   -- after an open: certain strike
      _       -> False

oracle :: [Int] -> Player
oracle tape h = tape !! length h == 0

----------------------------------------------------------------------- main
-- Training uses the SAME slope but a different intercept.  The factors of a
-- Sturmian word depend only on the slope, so this teaches the structure
-- without ever showing the player the stretch it is scored on.
row :: Int -> Int -> IO ()
row p0 m0 = do
  let runs = [ (sturm a rho, sturm a (rho + 0.37))
             | s <- [1..fights]
             , let a   = 0.52 + 0.10 * unit (lcg (fromIntegral s * 104729))
             , let rho = unit (lcg (fromIntegral s * 7919)) ]
      pct f = printf "%6.1f%%" (100 * fromIntegral (length [ () | (t,tr) <- runs, play p0 m0 t (f t tr) ])
                                / fromIntegral fights :: Double) :: String
  printf "  %4d %4d |%s%s%s%s%s%s%s%s\n" p0 m0
    (pct (\_ _  -> mash))       (pct (\_ _  -> certain))
    (pct (\_ _  -> afterH))     (pct (\_ tr -> raw 4 (take 6000 tr)))
    (pct (\_ tr -> raw 8 (take 6000 tr)))  (pct (\_ tr -> raw 12 (take 6000 tr)))
    (pct (\_ tr -> renorm 4 (take 6000 tr)))
    (pct (\t _  -> oracle t))

main :: IO ()
main = do
  printf "CHIP SCORING, NO CLOCK.  alpha in [0.52,0.62], %d fights per cell.\n" fights
  printf "block a strike %d, whiff %d, hit %d.\n\n" chipD whiffD hitD
  printf "  %4s %4s |%7s%8s%8s%8s%8s%8s%9s%8s\n"
         "you" "mon" "mash" "certain" "afterH" "raw4" "raw8" "raw12" "renorm4" "oracle"
  mapM_ (uncurry row) [(50,60),(55,60),(60,60),(70,60)]
  putStrLn ""
  putStrLn "  certain : swing only after HH -- guaranteed safe, and too slow to win"
  putStrLn "  afterH  : swing after any strike -- takes the split blind"
  putStrLn "  raw k   : majority-vote table over the last k beats (2^k contexts)"
  putStrLn "  renorm4 : the two certainties, plus a 4-deep table on the DERIVED word"
  putStrLn "            (16 contexts, and it beats raw12's 4096)"
