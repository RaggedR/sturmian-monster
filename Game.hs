-- STURMIAN MONSTER -- a console fighting game.
--
-- You block by default.  Tap SPACE during a beat to attack instead.
--
--   you attack, it is OPEN      -> it turns RED   and takes 2
--   you attack, it is STRIKING  -> you BOTH turn RED, you take 5
--   you block, it is STRIKING   -> you turn BLUE, you take 1   (chip)
--   you block, it is OPEN       -> nothing happens
--
-- There is no clock.  Blocking a strike costs 1, so stalling is paid for in
-- the same currency as everything else and the fight ends when somebody dies.
--
-- The monster gives no tell.  It looks identical every beat until the beat
-- resolves.  The only way to read it is the history strip: its attacks follow
-- a Sturmian sequence with a fresh irrational slope every game.
module Main where

import System.IO
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Text.Printf (printf)

------------------------------------------------------------------- settings
beatMicros, revealMicros :: Int
beatMicros   = 900000
revealMicros = 550000

-- Swept, not eyeballed (bench/Chip.hs).  At 60/60 the gradient is:
-- mash 0%, swing-only-after-HH 0% (safe but too slow to outrun the chip),
-- swing-after-any-strike 23%, memory-8 table 82%, renormalising 92%,
-- perfect prediction 100%.  No strategy dominates and none is free.
playerHP, monsterHP, hitDamage, whiffDamage, chipDamage, safetyCap :: Int
playerHP    = 60
monsterHP   = 60
hitDamage   = 2
whiffDamage = 5
chipDamage  = 1
safetyCap   = 5000

---------------------------------------------------------------------- colour
red, blue, yellow, green, cyan, dim, reset :: String
red    = "\ESC[1;31m"
blue   = "\ESC[1;94m"
yellow = "\ESC[1;33m"
green  = "\ESC[1;32m"
cyan   = "\ESC[36m"
dim    = "\ESC[2m"
reset  = "\ESC[0m"

---------------------------------------------------------------- the monster
lcg :: Integer -> Integer
lcg x = (1103515245 * x + 12345) `mod` 2147483648

unit :: Integer -> Double
unit r = fromIntegral (r `mod` 1000000) / 1000000

sturmian :: Double -> Double -> [Int]
sturmian a rho =
  [ floor (fromIntegral (k+1) * a + rho) - floor (fromIntegral k * a + rho)
  | k <- [0 :: Int ..] ]

cfOf :: Double -> [Int]
cfOf = go (12 :: Int)
  where go 0 _ = []
        go n x = let r = 1 / x; a = floor r in a : go (n-1) (r - fromIntegral a)

------------------------------------------------------------------ ascii art
data Beat = Waiting | Blocked | Clash | Connect | Missed deriving Eq

youBlock, youSwing :: [String]
youBlock =
  [ "      ___         "
  , "     ( o )        "
  , "    __|[#]        "
  , "      /|          "
  , "     /  \\         "
  , "    _/    \\_      " ]
youSwing =
  [ "      ___         "
  , "     (>o)         "
  , "    __|/====>     "
  , "      /|          "
  , "     /  \\         "
  , "    _/    \\_      " ]

monIdle, monStrike, monHurt :: [String]
monIdle =
  [ "     .-\"\"\"-.        "
  , "    / o   o \\       "
  , "   |    ^    |      "
  , "    \\  ---  /       "
  , "   __'-----'__      "
  , "  /  |     |  \\     " ]
monStrike =
  [ "  \\\\  .-\"\"\"-.       "
  , "   \\\\/ X   X \\       "
  , " <==|   >=<   |      "
  , "   //\\  ///  /       "
  , "  // __'---'__       "
  , "  /  |     |  \\     " ]
monHurt =
  [ "  *  .-\"\"\"-.  *     "
  , "    / x   x \\       "
  , "   |    o    |      "
  , "  * \\  ~~~  / *     "
  , "   __'-----'__      "
  , "  / |   |   | \\     " ]

-- (your art, your colour, your damage) and the same for the monster
scene :: Beat -> (([String], String, Int), ([String], String, Int))
scene Waiting = ((youBlock, green, 0), (monIdle,   cyan, 0))
scene Blocked = ((youBlock, blue,  chipDamage), (monStrike, cyan, 0))
scene Clash   = ((youSwing, red,   whiffDamage), (monStrike, red, 0))
scene Connect = ((youSwing, green, 0), (monHurt,   red, hitDamage))
scene Missed  = ((youBlock, green, 0), (monIdle,   cyan, 0))

---------------------------------------------------------------------- screen
bar :: Int -> Int -> String
bar full hp = col ++ "[" ++ replicate n '#' ++ dim ++ replicate (w - n) '.'
              ++ reset ++ col ++ "]" ++ reset
  where w   = 20                                   -- fixed width, whatever the pool
        n   = max 0 (min w ((hp * w + full - 1) `div` full))
        col | 100 * hp > 60 * full = green
            | 100 * hp > 25 * full = yellow
            | otherwise            = red

strip :: [Int] -> String
strip hist = concatMap sym (reverse (take 30 hist))
  where sym 0 = dim ++ " ." ++ reset
        sym _ = red ++ " H" ++ reset

dmgTag :: String -> Int -> String
dmgTag _   0 = "      "
dmgTag col d = col ++ printf "  -%d  " d ++ reset

draw :: Beat -> Int -> Int -> Int -> [Int] -> String -> IO ()
draw b php mhp left hist msg = do
  putStr "\ESC[2J\ESC[H"
  let ((yArt, yCol, yDmg), (mArt, mCol, mDmg)) = scene b
  printf "   YOU  %s %2d                MONSTER  %s %2d\n"
         (bar playerHP php) php (bar monsterHP mhp) mhp
  printf "        %-10s                       %-10s\n"
         (dmgTag red yDmg) (dmgTag red mDmg)
  mapM_ (\(y, m) -> putStrLn ("   " ++ yCol ++ y ++ reset ++ "      " ++ mCol ++ m ++ reset))
        (zip yArt mArt)
  printf "\n   history:%s %s\n" (strip hist)
         (if b == Waiting then dim ++ "?" ++ reset else " ")
  printf "\n   %s\n" msg
  printf "\n   %sbeat %d%s     [SPACE] attack   (do nothing = block)\n"
         dim (safetyCap - left) reset
  hFlush stdout

----------------------------------------------------------------------- input
listen :: Int -> IO (Bool, Bool)
listen total = go total False False
  where
    step = 20000
    go t hit quit
      | t <= 0 = return (hit, quit)
      | otherwise = do
          r <- hReady stdin
          if r then do c <- hGetChar stdin
                       go (t - step) (hit || c == ' ') (quit || c == 'q')
               else do threadDelay step
                       go (t - step) hit quit

------------------------------------------------------------------------ game
play :: Bool -> [Int] -> Int -> Int -> Int -> [Int] -> String -> IO String
play auto tape php mhp left hist msg
  | mhp <= 0  = return (green  ++ "YOU WIN -- the monster falls." ++ reset)
  | php <= 0  = return (red    ++ "YOU DIE -- it read you better than you read it." ++ reset)
  | left <= 0 = return (yellow ++ "STALEMATE -- neither of you can finish this." ++ reset)
  | otherwise = do
      draw Waiting php mhp left hist msg
      (hit, quit) <- if auto then autoMove hist else listen beatMicros
      if quit then return "you quit." else do
        let blow = head tape
            (beat, php', mhp', msg')
              | hit && blow == 0 = (Connect, php, mhp - hitDamage,
                                    red ++ "*** YOU CONNECT " ++ reset ++ "-- it takes " ++ show hitDamage)
              | hit              = (Clash, php - whiffDamage, mhp,
                                    red ++ "!!! YOU BOTH SWUNG " ++ reset ++ "-- you take " ++ show whiffDamage)
              | blow == 1        = (Blocked, php - chipDamage, mhp,
                                    blue ++ "### BLOCKED " ++ reset ++ "-- but it still costs "
                                    ++ show chipDamage)
              | otherwise        = (Missed, php, mhp,
                                    dim ++ "    it was open. you did nothing." ++ reset)
        draw beat php' mhp' (left-1) (blow : hist) msg'
        threadDelay revealMicros
        play auto (tail tape) php' mhp' (left-1) (blow : hist) msg'

autoMove :: [Int] -> IO (Bool, Bool)
autoMove hist = return (take 1 hist == [1], False)

-- ---------------- balance bench (no drawing) -------------------------------
-- Training uses the same slope with a different intercept.  A Sturmian word's
-- factors depend only on its slope, so this teaches the structure without ever
-- showing a player the stretch it is scored on (see LESSONS.md).
simulate :: ([Int] -> Bool) -> [Int] -> Bool
simulate decide tape = go playerHP monsterHP [] tape safetyCap
  where
    go _   mhp _ _ _ | mhp <= 0 = True
    go php _   _ _ _ | php <= 0 = False
    go _   _   _ _ 0            = False
    go php mhp hist (blow:rest) n
      | decide hist = if blow == 0 then go php (mhp - hitDamage)   (blow:hist) rest (n-1)
                                   else go (php - whiffDamage) mhp (blow:hist) rest (n-1)
      | otherwise   = if blow == 1 then go (php - chipDamage) mhp  (blow:hist) rest (n-1)
                                   else go php mhp                 (blow:hist) rest (n-1)
    go _ _ _ [] _ = False

-- For 1/2 < alpha < 2/3 there is no ".." and no "HHH", so of the three
-- length-2 contexts exactly one is undetermined:
--    H. -> H  certain strike      HH -> .  certain open      .H -> ?  the split
-- and the outcomes at the ".H" sites form a Sturmian word of their own.
deriveFwd, deriveBack :: [Int] -> [Int]
deriveFwd  w = [ c | (a,b,c) <- zip3 w (drop 1 w) (drop 2 w), a == 0, b == 1 ]
deriveBack h = [ c | (c,b,a) <- zip3 h (drop 1 h) (drop 2 h), a == 0, b == 1 ]

tableOf :: Int -> [Int] -> M.Map [Int] Int
tableOf k s = M.map (\(z,o) -> if z >= o then 0 else 1) (M.fromListWith plus
  [ (w, if nx == 0 then (1,0) else (0,1))
  | (w,nx) <- zip (map (take k) (iterate tail s)) (drop k s) ])
  where plus (a,b) (c,d) = (a+c, b+d)

rawPlayer :: Int -> [Int] -> [Int] -> Bool
rawPlayer k train = \h -> let w = take k h
                          in length w == k && M.findWithDefault 1 (reverse w) tb == 0
  where tb = tableOf k train

-- the two certainties, then recurse into the derived word at the split
renormPlayer :: Int -> [Int] -> [Int] -> Bool
renormPlayer k train = go
  where
    tb = tableOf k (deriveFwd train)
    go h = case h of
      (1:0:_) -> let dh = take k (deriveBack h)
                 in length dh == k && M.findWithDefault 1 (reverse dh) tb == 0
      (1:1:_) -> True
      (0:_)   -> False
      _       -> False

bench :: IO ()
bench = do
  let runs = [ (sturmian a rho, take 6000 (sturmian a (rho + 0.37)))
             | s <- [1 .. 300 :: Int]
             , let a   = 0.52 + 0.10 * unit (lcg (fromIntegral s * 104729))
             , let rho = unit (lcg (fromIntegral s * 7919)) ]
      pct f = printf "%5.1f%%"
                (100 * fromIntegral (length [ () | (t,tr) <- runs, simulate (f tr) t ])
                     / fromIntegral (length runs) :: Double) :: String
  printf "300 fights, alpha [0.52,0.62], you %d / monster %d HP, chip %d, whiff %d, no clock\n"
         playerHP monsterHP chipDamage whiffDamage
  printf "  mash attack every beat          : %s\n" (pct (const (const True)))
  printf "  swing only after HH (certain)   : %s\n" (pct (const (\h -> take 2 h == [1,1])))
  printf "  swing after any strike          : %s\n" (pct (const (\h -> take 1 h == [1])))
  printf "  memory-8 table                  : %s\n" (pct (rawPlayer 8))
  printf "  renormalising, depth 4          : %s\n" (pct (renormPlayer 4))

------------------------------------------------------------------------ main
main :: IO ()
main = do
  args <- getArgs
  let auto = "--demo" `elem` args
  if "--bench" `elem` args then bench else do
    t <- getCPUTime
    let r1    = lcg (t `div` 1000 + 7)
        r2    = lcg r1
        alpha = 0.52 + 0.10 * unit r1        -- ALWAYS above 1/2, so HH occurs and no attack rule is safe
        rho   = unit r2
        tape  = sturmian alpha rho
    unless auto $ do
      hSetBuffering stdin NoBuffering
      hSetEcho stdin False
      putStr "\ESC[2J\ESC[H"
      putStrLn ("\n   " ++ cyan ++ "A monster you have never met. It gives no tell." ++ reset)
      putStrLn "   Blocking costs you 1. Swinging into a strike costs you 5."
      putStrLn "   There is no clock -- waiting is a choice you pay for."
      putStrLn ("\n   " ++ dim ++ "press SPACE to begin, q to quit" ++ reset)
      _ <- hGetChar stdin
      return ()
    result <- play auto tape playerHP monsterHP safetyCap [] "the fight begins."
    putStrLn ("\n   " ++ result)
    printf "\n   %sit was Sturmian, slope alpha = %.6f%s\n" dim alpha reset
    printf "   %scontinued fraction [0; %s ...]%s\n" dim (intercalate ", " (map show (cfOf alpha))) reset
    printf "   %s(the partial quotients are how long it looks solved before it isn't)%s\n\n" dim reset
    unless auto $ hSetEcho stdin True
