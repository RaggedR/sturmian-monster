-- THE GATE.  Measured before any of the game is built, because LESSONS.md says
-- to test the headline claim against the metric before building on it.
--
-- The world is the Rauzy graph of a Sturmian word (see Rauzy.hs): m+1 rooms,
-- m+2 doors, exactly one room with a choice.  You walk it; the bit a door
-- emits is the monster's action on that beat.  The map is hidden, and blocking
-- through a door costs nothing but tempo, so scouting is free except in time.
--
-- Three questions:
--   A. is there a dominant strategy?          (if yes, this is not a game)
--   B. does scouting the far arc pay?         (if never, the choice is worthless)
--   C. does the ceiling match the prediction? (best cycle density = a best
--                                              rational approximation to 1-alpha)
module Main where

import qualified Data.Map.Strict as M
import Data.List (sortOn, nub, minimumBy, intercalate)
import Data.Ord (comparing)
import Text.Printf (printf)

------------------------------------------------------------------- settings
phpDefault, hitD, whiffD :: Int
phpDefault = 20; hitD = 2; whiffD = 5

fights :: Int
fights = 300

--------------------------------------------------------------------- words
sturmian :: Double -> Double -> [Int]
sturmian a rho =
  [ floor (fromIntegral (k+1)*a + rho) - floor (fromIntegral k*a + rho) | k <- [0::Int ..] ]

lcg :: Integer -> Integer
lcg x = (1103515245*x + 12345) `mod` 2147483648

unit :: Integer -> Double
unit r = fromIntegral (r `mod` 1000000) / 1000000

--------------------------------------------------------------------- graph
type Room  = [Int]
type Graph = M.Map Room [(Int, Room)]      -- room -> doors, in a fixed order

-- Doors are ordered by a hash of (room, destination).  Stable across the
-- fight, so the player can refer to "the first door", and independent of the
-- bit, so the order gives nothing away.
doorKey :: Room -> Room -> Int
doorKey u v = (17 * sum (zipWith (*) [1..] u) + 131 * sum (zipWith (*) [3,5..] v)) `mod` 1009

rauzy :: Int -> [Int] -> Graph
rauzy m w = M.map (sortOn snd . nub) (M.fromListWith (++)
  [ (take m ws, [(ws !! m, take m (tail ws))]) | ws <- take 6000 (iterate tail w) ])
  |> M.mapWithKey (\u es -> sortOn (doorKey u . snd) es)
  where x |> f = f x

branchOf :: Graph -> Maybe Room
branchOf g = case [ v | (v,es) <- M.toList g, length es == 2 ] of
  [b] -> Just b
  _   -> Nothing

-- walk out of the branch room through a door and round until we are back.
-- every room between has one door, so nothing is decided on the way.
arc :: Graph -> Room -> (Int, Room) -> (Int, Int)     -- (open beats, beats)
arc g b (bit, v) = go v (1 - bit) 1
  where go u z l | u == b    = (z, l)
                 | otherwise = case M.findWithDefault [] u g of
                     [(c, u')] -> go u' (z + 1 - c) (l + 1)
                     _         -> (z, l)

-- the best sustainable damage rate: the densest cycle.  On a necklace the only
-- simple cycles are the two through the branch room, so this is Karp by hand.
bestArc :: Graph -> Maybe (Int, (Int, Int))
bestArc g = do
  b <- branchOf g
  let ds = M.findWithDefault [] b g
      ws = [ (i, arc g b d) | (i, d) <- zip [0..] ds ]
  pure (minimumBy (comparing (\(_, (z,l)) -> negate (fromIntegral z / fromIntegral l :: Double))) ws)

-------------------------------------------------------------------- players
type Known  = M.Map (Room, Int) Int
type Policy = Room -> Known -> (Int, Bool)     -- which door, and attack?

-- attack everything, always the first door
mash :: Policy
mash _ _ = (0, True)

-- attack unknown doors as well as known-open ones: reckless
blind :: Graph -> Policy
blind g v k = case unexplored g v k of
  (i:_) -> (i, True)
  []    -> (pickOpen g v k, openAt g v k)

-- scout freely (blocking is free), attack only doors known to be open.
-- At the branch room, only ever take the door it first walked -- it never
-- looks at the far arc.
firstArc :: Graph -> Policy
firstArc g v k
  | Just i <- committed g v k = (i, M.findWithDefault 1 (v,i) k == 0)
  | (i:_) <- unexplored g v k = (i, False)
  | otherwise                 = (pickOpen g v k, openAt g v k)
  where
    committed gg vv kk
      | length (M.findWithDefault [] vv gg) == 2
      , [i] <- [ j | j <- [0,1], M.member (vv,j) kk ] = Just i
      | otherwise = Nothing

-- scout both arcs once, then commit to the denser cycle for the rest of the
-- fight.  Reading the arc off the graph after walking it is exactly what
-- remembering the walk would give you.
bothArcs :: Graph -> Policy
bothArcs g v k
  | length ds == 2, all (\j -> M.member (v,j) k) [0,1] = let i = pref in (i, bitAt i == 0)
  | (i:_) <- unexplored g v k = (i, False)
  | otherwise = (pickOpen g v k, openAt g v k)
  where ds   = M.findWithDefault [] v g
        pref = fst (minimumBy (comparing (\(_, (z,l)) -> negate (fromIntegral z / fromIntegral l :: Double)))
                      [ (i, arc g v d) | (i,d) <- zip [0..] ds ])
        bitAt i = fst (ds !! i)

-- knows the map from the first beat: the ceiling
oracle :: Graph -> Policy
oracle g v _ = (i, fst (ds !! i) == 0)
  where ds = M.findWithDefault [] v g
        i  = case (length ds, bestArc g, branchOf g) of
               (2, Just (j,_), Just b) | b == v -> j
               _                                -> 0

unexplored :: Graph -> Room -> Known -> [Int]
unexplored g v k = [ i | i <- [0 .. length (M.findWithDefault [] v g) - 1], not (M.member (v,i) k) ]

pickOpen :: Graph -> Room -> Known -> Int
pickOpen g v k = case [ i | i <- [0 .. length (M.findWithDefault [] v g) - 1]
                          , M.findWithDefault 1 (v,i) k == 0 ] of
  (i:_) -> i
  []    -> 0

openAt :: Graph -> Room -> Known -> Bool
openAt g v k = let i = pickOpen g v k in M.findWithDefault 1 (v,i) k == 0

------------------------------------------------------------------ the fight
fight :: Graph -> Room -> Int -> Int -> Policy -> Bool
fight g start beats mhp0 pol = go start phpDefault mhp0 M.empty beats
  where
    go v php mhp k left
      | mhp <= 0  = True
      | php <= 0  = False
      | left <= 0 = False
      | otherwise =
          let ds        = M.findWithDefault [] v g
              (i, atk)  = pol v k
              i'        = min i (length ds - 1)
              (bit, nx) = ds !! i'
              php'      = if atk && bit == 1 then php - whiffD else php
              mhp'      = if atk && bit == 0 then mhp - hitD   else mhp
          in go nx php' mhp' (M.insert (v,i') bit k) (left - 1)

-- one world per seed: a slope, an offset, and where you woke up
world :: Int -> Int -> (Double, Graph, Room)
world m s = (alpha, g, start)
  where alpha = 0.25 + 0.50 * unit (lcg (fromIntegral s * 104729))
        rho   = unit (lcg (fromIntegral s * 7919))
        g     = rauzy m (sturmian alpha rho)
        rooms = M.keys g
        start = rooms !! (fromIntegral (lcg (fromIntegral s * 31337) `mod` fromIntegral (length rooms)))

winRate :: Int -> Int -> Int -> (Graph -> Policy) -> Double
winRate m beats mhp mk =
  100 * fromIntegral (length [ () | s <- [1..fights], let (_,g,st) = world m s
                                  , fight g st beats mhp (mk g) ])
      / fromIntegral fights

------------------------------------------------------- rational approximation
bestSide :: (Double -> Double -> Bool) -> Double -> Int -> (Int, Int)
bestSide cmp x qm = minimumBy (comparing err)
  [ (p,q) | q <- [1..qm], p <- [0..q], cmp (fromIntegral p / fromIntegral q) x ]
  where err (p,q) = abs (fromIntegral p / fromIntegral q - x)

showF :: (Int,Int) -> String
showF (p,q) = show p ++ "/" ++ show q

reduce :: (Int,Int) -> (Int,Int)
reduce (p,q) = let d = gcd p q in if d == 0 then (p,q) else (p `div` d, q `div` d)


-- Total damage dealt in `beats`, with no HP pool to clear.  Win rates hide the
-- size of an effect behind a threshold; this does not.
damageOf :: Graph -> Room -> Int -> Policy -> Double
damageOf g start beats pol = go start phpDefault M.empty beats 0
  where
    go v php k left acc
      | php <= 0 || left <= 0 = acc
      | otherwise =
          let ds        = M.findWithDefault [] v g
              (i, atk)  = pol v k
              i'        = min i (length ds - 1)
              (bit, nx) = ds !! i'
          in go nx (if atk && bit == 1 then php - whiffD else php)
                   (M.insert (v,i') bit k) (left - 1)
                   (if atk && bit == 0 then acc + fromIntegral hitD else acc)

meanDamage :: Int -> Int -> (Graph -> Policy) -> Double
meanDamage m beats mk =
  sum [ damageOf g st beats (mk g) | s <- [1..fights], let (_,g,st) = world m s ]
  / fromIntegral fights

-- how far apart the two arcs are, in open beats per beat
meanGap :: Int -> Double
meanGap m = avg [ abs (d 0 - d 1)
                | s <- [1..fights], let (_,g,_) = world m s
                , Just b <- [branchOf g]
                , let ds = M.findWithDefault [] b g, length ds == 2
                , let d i = let (z,l) = arc g b (ds !! i)
                            in fromIntegral z / fromIntegral l ]
  where avg xs = sum xs / fromIntegral (max 1 (length xs))

----------------------------------------------------------------------- main
main :: IO ()
main = do
  let m0 = 8; beats0 = 120; mhp0 = 120
  printf "%d fights per cell, alpha in [0.25,0.75], you %d HP, hit %d, whiff %d\n"
         fights phpDefault hitD whiffD

  printf "\nA.  IS THERE A DOMINANT STRATEGY?   m=%d, monster %d HP, %d beats\n\n" m0 mhp0 beats0
  mapM_ (\(nm, mk) -> printf "      %-16s %5.1f%%\n" (nm::String) (winRate m0 beats0 mhp0 mk))
    [ ("mash",            const mash)
    , ("blind gambler",   blind)
    , ("first arc only",  firstArc)
    , ("scouts both",     bothArcs)
    , ("oracle (Karp)",   oracle) ]

  printf "\nB.  DOES SCOUTING THE FAR ARC PAY?   monster %d HP, %d beats\n\n" mhp0 beats0
  printf "      %-6s %-11s %-11s %-11s %s\n" "m" "first arc" "scouts both" "oracle" "scouting is worth"
  mapM_ (\m -> do
            let f = winRate m beats0 mhp0 firstArc
                b = winRate m beats0 mhp0 bothArcs
                o = winRate m beats0 mhp0 oracle
            printf "      %-6d %-11s %-11s %-11s %+5.1f\n" m
                   (pc f) (pc b) (pc o) (b - f))
        [3, 5, 8, 13, 21 :: Int]

  printf "\nC.  DOES THE CEILING MATCH THE PREDICTION?\n"
  printf "    the two cycle densities should be the best rational approximations\n"
  printf "    to 1-alpha from each side with denominator at most m.\n\n"
  printf "      %-6s %s\n" "m" "agreement over 300 slopes"
  mapM_ (\m -> do
            let hits = length [ () | s <- [1..fights]
                              , let (alpha, g, _) = world m s
                              , Just b <- [branchOf g]
                              , let ds = [ reduce (arc g b d) | d <- M.findWithDefault [] b g ]
                              , let x  = 1 - alpha
                              , let hi = bestSide (>=) x (m+1)
                              , let lo = bestSide (<=) x (m+1)
                              , sortOn snd ds == sortOn snd (nub [reduce hi, reduce lo]) ]
            printf "      %-6d %d/%d\n" m hits fights)
        [3, 5, 8, 13, 21 :: Int]

  printf "\n    a worked slope at m=8:\n"
  mapM_ (\s -> do
            let (alpha, g, _) = world 8 s
            case branchOf g of
              Nothing -> pure ()
              Just b  -> do
                let ds = [ reduce (arc g b d) | d <- M.findWithDefault [] b g ]
                    x  = 1 - alpha
                printf "      alpha %.4f  1-alpha %.4f   cycles %s   best approx %s and %s\n"
                       alpha x (intercalate ", " (map showF ds))
                       (showF (reduce (bestSide (>=) x 9))) (showF (reduce (bestSide (<=) x 9))))
        [1, 2, 3, 4 :: Int]

  printf "\nD.  WHAT IS THE DOOR ACTUALLY WORTH?   mean damage over %d beats,\n" beats0
  printf "    with no HP pool, so nothing is hidden behind a win threshold.\n\n"
  printf "      %-6s %-11s %-11s %-11s %-11s %s\n" "m" "first arc" "scouts both" "oracle" "arc gap" "doors differ by"
  mapM_ (\m -> do
            let f = meanDamage m beats0 firstArc
                b = meanDamage m beats0 bothArcs
                o = meanDamage m beats0 oracle
                gp = meanGap m
            printf "      %-6d %-11.1f %-11.1f %-11.1f %-11.4f %.1f dmg\n"
                   m f b o gp (gp * 2 * fromIntegral beats0))
        [3, 5, 8, 13, 21 :: Int]

  putStrLn ""
  where pc v = printf "%5.1f%%" (v :: Double) :: String
