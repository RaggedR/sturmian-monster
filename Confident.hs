-- Two changes to the player.
--
-- 1. CONFIDENCE.  The table stores COUNTS, not the last thing seen:
--       window  ->  (how many times a light followed, how many times a heavy)
--    Attack deals 3; a light costs 1, a heavy costs 6.  So attacking into a
--    light is worth +2 and into a heavy -3, and the player should attack only
--    when  2*lights - 3*heavies > 0.  He starts with one phantom heavy on the
--    books, so an unseen window says "block" and one lucky observation is not
--    enough to tempt him.  Uncertainty is now priced, not guessed.
--
-- 2. WHERE INFORMATION COMES FROM.  If you only see the monster clearly while
--    you are guarding (learnWhileAttacking = False), studying is a real
--    purchase and Robin's study phase should show an optimum.  If you see
--    every blow regardless, it should not.
module Main where

import qualified Data.Map.Strict as M
import Data.List (maximumBy)
import Data.Ord (comparing)
import Text.Printf (printf)

horizon, len :: Int
horizon = 2000
len     = 100000

periodic, sturmian, randomW :: [Int]
periodic = take len (cycle [0,1,1,0])
sturmian = take len (head (dropWhile ((< len) . length) (iterate grow [0])))
  where grow = concatMap (\c -> if c == 0 then [0,1] else [0])
randomW  = take len (map (\x -> fromIntegral ((x `div` 65536) `mod` 2)) (iterate lcg 42))
  where lcg x = (1103515245 * x + 12345) `mod` (2147483648 :: Integer)

cost :: Int -> Int
cost 0 = 1
cost _ = 6

data Rule = LastSeen | Confident deriving Eq

run :: Rule -> Bool -> Int -> Int -> [Int] -> Int          -- returns net score
run rule learnWhileAttacking n studyFor w = go M.empty [] 0 (take horizon w) 0
  where
    go _ _ _ [] acc = acc
    go tbl hist k (blow : rest) acc =
      let haveWin  = length hist >= n
          win      = reverse (take n hist)
          (l, h)   = M.findWithDefault (0,0) win tbl
          willing  = case rule of
                       LastSeen  -> (l, h) /= (0,0) && l >= h      -- last-seen-ish: no caution
                       Confident -> 2*l - 3*(h+1) > 0              -- expected value, pessimistic
          attacking = k >= studyFor && haveWin && willing
          acc'      = if attacking then acc + 3 - cost blow else acc
          learning  = haveWin && (learnWhileAttacking || not attacking)
          tbl'      = if learning
                        then M.insertWith (\_ (a,b) -> if blow == 0 then (a+1,b) else (a,b+1))
                                          win (if blow == 0 then (1,0) else (0,1)) tbl
                        else tbl
      in go tbl' (blow : hist) (k+1) rest acc'

ceilingOf :: [Int] -> Int
ceilingOf w = 2 * length (filter (== 0) (take horizon w))   -- attack every light, block every heavy

monsters :: [(String, [Int])]
monsters = [("Sturmian", sturmian), ("periodic 0110", periodic), ("random", randomW)]

main :: IO ()
main = do
  putStrLn ("PART 1 -- does pricing uncertainty help?  (net score over "
            ++ show horizon ++ " ticks, learning always, no study phase)")
  putStrLn "                     memory: "
  putStrLn ("  monster        rule  " ++ concatMap (printf "%7d") [1..12::Int] ++ "   ceiling")
  mapM_ (\(nm, w) -> do
            line nm "last-seen" (\n -> run LastSeen  True n 0 w)
            line "" "confident" (\n -> run Confident True n 0 w)
            printf "%58s%10d\n" "" (ceilingOf w)) monsters
  putStrLn ""
  putStrLn "  (blocking forever scores 0 -- any negative number is worse than doing nothing)"
  putStrLn ""
  putStrLn "PART 2 -- now you can ONLY learn while blocking.  Does studying pay?"
  mapM_ grid monsters
  where
    line nm r f = putStrLn (printf "  %-14s %-5s " nm r ++ concatMap (printf "%7d" . f) [1..12::Int])
    grid (nm, w) = do
      printf "=== %s ===  net score, learning only while blocking\n" nm
      putStrLn ("  n \\ study " ++ concatMap (printf "%7d") studies)
      mapM_ (\n -> putStrLn (printf "%5d     " n
                    ++ concatMap (\s -> printf "%7d" (run Confident False n s w)) studies)) [1..12::Int]
      let (bn, bs, bv) = maximumBy (comparing (\(_,_,v) -> v))
                           [ (n, s, run Confident False n s w) | n <- [1..12::Int], s <- studies ]
      printf "  best: memory %d, study %d  ->  net %d   (ceiling %d)\n\n" bn bs bv (ceilingOf w)
    studies = [0, 25, 50, 100, 200, 400, 800] :: [Int]
