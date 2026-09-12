-- STURMIAN MONSTER -- a console fighting game.
--
-- You block by default.  Tap SPACE during a beat to attack instead.
--
--   you attack, it is OPEN      -> it turns RED   and takes 2
--   you attack, it is STRIKING  -> you BOTH turn RED, you take 5
--   you block, it is STRIKING   -> you turn BLUE, nothing happens
--   you block, it is OPEN       -> nothing happens
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

-- Tuned by sweep, not by eye (see LESSONS.md). The monster's HP sets the clock
-- pressure, your HP sets the mistake budget, and they want different values:
-- 60 HP is 30 hits, well past the ~17 certain hits a 120-beat fight offers, so
-- "wait for HH" cannot win on time. 20 HP is four whiffs.
playerHP, monsterHP, beats, hitDamage, whiffDamage :: Int
playerHP    = 20
monsterHP   = 60
beats       = 120
hitDamage   = 2
whiffDamage = 5

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
scene Blocked = ((youBlock, blue,  0), (monStrike, cyan, 0))
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
  printf "\n   %sbeats left: %2d%s     [SPACE] attack   (do nothing = block)\n" dim left reset
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
  | left <= 0 = return (yellow ++ "OUT OF TIME -- it walks away, bored." ++ reset)
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
              | blow == 1        = (Blocked, php, mhp,
                                    blue ++ "### BLOCKED " ++ reset ++ "-- you took nothing")
              | otherwise        = (Missed, php, mhp,
                                    dim ++ "    it was open. you did nothing." ++ reset)
        draw beat php' mhp' (left-1) (blow : hist) msg'
        threadDelay revealMicros
        play auto (tail tape) php' mhp' (left-1) (blow : hist) msg'

autoMove :: [Int] -> IO (Bool, Bool)
autoMove hist = return (take 1 hist == [1], False)

-- ---------------- balance bench (no drawing): four kinds of player -------------
table4 :: [Int] -> M.Map [Int] Int
table4 w = M.map (\(z,o) -> if z >= o then 0 else 1) (M.fromListWith plus
  [ (take 4 (drop i w), if w !! (i+4) == 0 then (1,0) else (0,1)) | i <- [0 .. 3000] ])
  where plus (a,b) (c,d) = (a+c, b+d)

simulate :: ([Int] -> Bool) -> [Int] -> String
simulate decide tape = go playerHP monsterHP [] tape beats
  where
    go php mhp hist (blow:rest) left
      | mhp <= 0  = "win"
      | php <= 0  = "die"
      | left <= 0 = "time"
      | otherwise =
          let hit = decide hist
              php' = if hit && blow == 1 then php - whiffDamage else php
              mhp' = if hit && blow == 0 then mhp - hitDamage else mhp
          in go php' mhp' (blow:hist) rest (left-1)
    go _ _ _ [] _ = "time"

bench :: IO ()
bench = do
  let runs = [ (a, sturmian a (unit (lcg (fromIntegral s * 7919))))
             | s <- [1 .. 300 :: Int]
             , let a = 0.52 + 0.10 * unit (lcg (fromIntegral s * 104729)) ]
      pct f = printf "%5.1f%%" (100 * fromIntegral (length [ () | (a,t) <- runs, f a t == "win" ])
                                / fromIntegral (length runs) :: Double) :: String
  printf "300 fights, alpha [0.52,0.62], you %d / monster %d HP, %d beats, whiff %d\n" playerHP monsterHP beats whiffDamage
  printf "  mash attack every beat         : %s\n" (pct (\_ t -> simulate (const True) t))
  printf "  attack right after a strike    : %s\n" (pct (\_ t -> simulate (\h -> take 1 h == [1]) t))
  printf "  ..same, but gambles on beat 1  : %s\n" (pct (\_ t -> simulate (\h -> take 1 h /= [0]) t))
  printf "  studied the monster (memory 4) : %s\n"
         (pct (\_ t -> let tb = table4 t
                       in simulate (\h -> length h >= 4
                            && M.findWithDefault 1 (reverse (take 4 h)) tb == 0) t))

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
      putStrLn "   Blocking is free. Swinging into a strike is not."
      putStrLn ("\n   " ++ dim ++ "press SPACE to begin, q to quit" ++ reset)
      _ <- hGetChar stdin
      return ()
    result <- play auto tape playerHP monsterHP beats [] "the fight begins."
    putStrLn ("\n   " ++ result)
    printf "\n   %sit was Sturmian, slope alpha = %.6f%s\n" dim alpha reset
    printf "   %scontinued fraction [0; %s ...]%s\n" dim (intercalate ", " (map show (cfOf alpha))) reset
    printf "   %s(the partial quotients are how long it looks solved before it isn't)%s\n\n" dim reset
    unless auto $ hSetEcho stdin True
