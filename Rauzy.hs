-- THE RAUZY GRAPH -- the world the directed-container game is played in.
--
-- Take a Sturmian word.  Its factors of length m are the ROOMS.  There is a
-- door from room u to room v when u = a.s and v = s.b and a.s.b is a factor;
-- the door EMITS the bit b.  Walking the graph therefore emits a bit sequence,
-- and the room you are standing in IS the last m bits you emitted.
--
-- Three facts, checked below rather than asserted:
--
--   (1) there are exactly m+1 rooms and m+2 doors;
--   (2) exactly ONE room has two doors, and they emit different bits;
--   (3) the two cycles through that room have open-densities that bracket
--       1-alpha, and each is the best rational approximation to 1-alpha on
--       its own side with denominator at most m.
--
-- (1) is the Sturmian complexity function p(m) = m+1 wearing a different hat:
-- the "one split at every window length" of splitting.pdf is one BRANCHING
-- ROOM.  (3) is why the choice at that room is the whole game.
module Main where

import qualified Data.Map.Strict as M
import Data.List (nub, sortOn, intercalate, minimumBy)
import Data.Ord (comparing)
import Text.Printf (printf)

--------------------------------------------------------------------- words
sturmian :: Double -> Double -> [Int]
sturmian a rho =
  [ floor (fromIntegral (k+1) * a + rho) - floor (fromIntegral k * a + rho)
  | k <- [0 :: Int ..] ]

lcg :: Integer -> Integer
lcg x = (1103515245 * x + 12345) `mod` 2147483648

unit :: Integer -> Double
unit r = fromIntegral (r `mod` 1000000) / 1000000

-- continued fraction of x in (0,1), as [a1,a2,...]
cfOf :: Int -> Double -> [Int]
cfOf 0 _ = []
cfOf n x
  | x < 1e-12 = []
  | otherwise = let r = 1 / x; a = floor r in a : cfOf (n-1) (r - fromIntegral a)

-- convergents h/k of [0; a1, a2, ...]
convergents :: [Int] -> [(Int, Int)]
convergents = go (1,0) (0,1)
  where go (h1,h2) (k1,k2) (a:rest) = let h = a*h1 + h2; k = a*k1 + k2
                                      in (h,k) : go (h,h1) (k,k1) rest
        go _ _ [] = []

--------------------------------------------------------------------- graph
type Room  = [Int]
type Graph = M.Map Room [(Int, Room)]     -- room -> [(bit the door emits, where it leads)]

prefixLen :: Int
prefixLen = 8000

rauzy :: Int -> [Int] -> Graph
rauzy m w = M.map (sortOn fst . nub) (M.fromListWith (++)
  [ (take m ws, [(ws !! m, take m (tail ws))])
  | ws <- take prefixLen (iterate tail w) ])

doors :: Graph -> Int
doors = sum . map length . M.elems

branches :: Graph -> [Room]
branches g = [ v | (v, es) <- M.toList g, length es >= 2 ]

-- from the branch room, out through one door, round until we are back.
-- every room in between has exactly one door, so there is nothing to decide.
loop :: Graph -> Room -> (Int, Room) -> [(Room, Int, Room)]
loop g b (bit, v) = (b, bit, v) : go v
  where go u | u == b    = []
             | otherwise = case M.findWithDefault [] u g of
                 [(b', u')] -> (u, b', u') : go u'
                 _          -> []          -- not a necklace; caller reports it

-- (beats, open beats) of a cycle
weigh :: [(Room, Int, Room)] -> (Int, Int)
weigh es = (length es, length [ () | (_, b, _) <- es, b == 0 ])

-- best rational approximation to x with denominator <= q, on a given side
bestUnder, bestOver :: Double -> Int -> (Int, Int)
bestUnder x qm = pick [ (p,q) | q <- [1..qm], p <- [0..q], fromIntegral p / fromIntegral q <= x ] x
bestOver  x qm = pick [ (p,q) | q <- [1..qm], p <- [0..q], fromIntegral p / fromIntegral q >= x ] x

pick :: [(Int,Int)] -> Double -> (Int,Int)
pick cands x = minimumBy (comparing err) cands
  where err (p,q) = abs (fromIntegral p / fromIntegral q - x)

--------------------------------------------------------------------- names
names :: Int -> [Int] -> Graph -> M.Map Room String
names m w g = M.fromList (zip order [ "r" ++ show i | i <- [0 :: Int ..] ])
  where seen  = [ take m ws | ws <- take prefixLen (iterate tail w) ]
        order = nub (filter (`M.member` g) seen)

nameOf :: M.Map Room String -> Room -> String
nameOf nm r = M.findWithDefault "??" r nm

bits :: Room -> String
bits = concatMap (\b -> if b == 1 then "H" else ".")

--------------------------------------------------------------------- report
detail :: Double -> Double -> Int -> IO ()
detail alpha rho m = do
  let w  = sturmian alpha rho
      g  = rauzy m w
      nm = names m w g
      bs = branches g
  printf "\n  alpha = %.6f    1-alpha = %.6f    order m = %d\n" alpha (1-alpha) m
  printf "  word  %s...\n\n" (bits (take 40 w))
  printf "  rooms %d (m+1 = %d)     doors %d (m+2 = %d)     branching rooms %d\n\n"
         (M.size g) (m+1) (doors g) (m+2) (length bs)
  putStrLn "  the map (a room IS the last m bits you emitted):"
  mapM_ (\(v, es) -> printf "    %-4s %s  %s\n" (nameOf nm v) (bits v)
           (intercalate "   " [ printf "-%s-> %s" (if b == 1 then "H" else "." :: String)
                                                  (nameOf nm u) | (b, u) <- es ]))
        (sortOn (nameOf nm . fst) (M.toList g))
  case bs of
    [b] -> do
      printf "\n  the one room with a choice: %s\n" (nameOf nm b)
      let es = M.findWithDefault [] b g
      mapM_ (\d@(bit, _) -> do
                let cyc = loop g b d
                    (len, zs) = weigh cyc
                printf "    door %s : %2d beats, %2d open   density %d/%d = %.4f\n"
                       (if bit == 1 then "H" else "." :: String) len zs zs len
                       (fromIntegral zs / fromIntegral len :: Double)
                printf "             %s\n"
                       (intercalate " " [ printf "%s-%s->%s" (nameOf nm u)
                                            (if c == 1 then "H" else "." :: String)
                                            (nameOf nm v) | (u, c, v) <- cyc ]))
            es
      let ds = [ let (l, z) = weigh (loop g b d) in (z, l) | d <- es ]
          x  = 1 - alpha
      printf "\n    1-alpha = %.4f sits between them: %s\n" x
             (intercalate " and " [ printf "%d/%d = %.4f" z l
                                      (fromIntegral z / fromIntegral l :: Double)
                                  | (z, l) <- ds ])
      printf "    best approximation to 1-alpha with q <= %d, from above: %s ; below: %s\n"
             m (showF (bestOver x m)) (showF (bestUnder x m))
      printf "    convergents of 1-alpha: %s\n"
             (intercalate ", " (map showF (take 7 (convergents (0 : cfOf 10 x)))))
    _ -> printf "\n  NOT A NECKLACE: %d branching rooms\n" (length bs)

showF :: (Int, Int) -> String
showF (p, q) = show p ++ "/" ++ show q

--------------------------------------------------------------------- sweep
sweep :: IO ()
sweep = do
  putStrLn "\n  SWEEP -- 40 slopes x 6 orders. rooms should be m+1, doors m+2, branches 1.\n"
  printf "   %-6s %-14s %-14s %s\n" "m" "rooms = m+1?" "doors = m+2?" "exactly one branch?"
  mapM_ (\m -> do
            let gs = [ rauzy m (sturmian a (unit (lcg (fromIntegral s * 7919))))
                     | s <- [1 .. 40 :: Int]
                     , let a = 0.25 + 0.50 * unit (lcg (fromIntegral s * 104729)) ]
                ok f = length (filter f gs)
            printf "   %-6d %-14s %-14s %s\n" m
                   (tally (ok ((== m+1) . M.size)))
                   (tally (ok ((== m+2) . doors)))
                   (tally (ok ((== 1) . length . branches))))
        [3, 5, 8, 13, 17, 21 :: Int]
  where tally n = printf "%d/40" (n :: Int) :: String

--------------------------------------------------------------------- main
main :: IO ()
main = do
  putStrLn "=== THE RAUZY GRAPH OF A STURMIAN WORD ============================"
  detail 0.367799 0.412 5
  detail 0.367799 0.412 8
  detail 0.612300 0.735 8
  sweep
  putStrLn ""
