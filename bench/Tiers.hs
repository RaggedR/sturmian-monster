module Main where
import qualified Data.Map.Strict as M
import Data.List (group)
import Text.Printf (printf)

php0, mhp0, beats, hitD, whiffD :: Int
php0 = 20; mhp0 = 60; beats = 120; hitD = 2; whiffD = 5

lcg :: Integer -> Integer
lcg x = (1103515245 * x + 12345) `mod` 2147483648
unit :: Integer -> Double
unit r = fromIntegral (r `mod` 1000000) / 1000000
sturm :: Double -> Double -> [Int]
sturm a rho = [ floor (fromIntegral (k+1)*a+rho) - floor (fromIntegral k*a+rho) | k <- [0::Int ..] ]

sim :: ([Int] -> Bool) -> [Int] -> Bool
sim decide tape = go php0 mhp0 [] tape beats
  where
    go php mhp hist (b:rest) left
      | mhp <= 0  = True
      | php <= 0  = False
      | left <= 0 = False
      | otherwise = let h = decide hist
                    in go (if h && b==1 then php-whiffD else php)
                          (if h && b==0 then mhp-hitD  else mhp) (b:hist) rest (left-1)
    go _ _ _ [] _ = False

tableN :: Int -> [Int] -> M.Map [Int] Int
tableN n pre = M.map (\(z,o)->if z>=o then 0 else 1) (M.fromListWith plus
  [ (w, if nx==0 then (1,0) else (0,1))
  | (w,nx) <- zip (map (take n) (takeWhile ((>=n).length) (iterate tail pre))) (drop n pre) ])
  where plus (a,b) (c,d) = (a+c,b+d)
mem :: Int -> M.Map [Int] Int -> [Int] -> Bool
mem n tb h = length h >= n && M.findWithDefault 1 (reverse (take n h)) tb == 0

tier :: (String, Double, Double) -> IO ()
tier (nm, lo, hi) = do
  let ts = [ sturm a (unit (lcg (fromIntegral s*7919)))
           | s <- [1..200::Int], let a = lo + (hi-lo) * unit (lcg (fromIntegral s*104729)) ]
      pct f = printf "%4.0f%%" (100 * fromIntegral (length (filter id (map f ts))) / 200 :: Double) :: String
      runs t = maximum (map length (filter ((==1).head) (group (take 400 t))))
      certainty t = let w = take 400 t; r = runs t
                    in fromIntegral (length [ () | i <- [0..length w-r-1]
                                            , all (==1) (take r (drop i w)) ]) / 400 :: Double
  printf "  %-22s %5s %9s %9s %7s   run<=%d  certain %.0f%% of beats\n" nm
    (pct (sim (const True))) (pct (sim (\h -> take 1 h == [1])))
    (pct (sim (\h -> take 2 h == [1,1])))
    (pct (\t -> sim (mem 8 (tableN 8 (take 4000 (drop 6000 t)))) t))
    (runs (head ts))
    (100 * (sum (map certainty ts) / 200))

main :: IO ()
main = do
  printf "you %d HP, monster %d HP, %d beats.  200 fights per tier\n\n" php0 mhp0 beats
  putStrLn "  tier                    mash  after-H   after-HH   mem8"
  mapM_ tier [ ("trash   a in .35-.45", 0.35, 0.45)
             , ("regular a in .52-.62", 0.52, 0.62)
             , ("elite   a in .63-.66", 0.63, 0.66)
             , ("boss    a in .70-.78", 0.70, 0.78) ]
