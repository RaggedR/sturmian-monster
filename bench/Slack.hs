module Main where
import qualified Data.Map.Strict as M
import Text.Printf (printf)
php0,mhp0,hitD,whiffD :: Int
php0=20; mhp0=60; hitD=2; whiffD=5
lcg :: Integer -> Integer
lcg x = (1103515245*x+12345) `mod` 2147483648
unit :: Integer -> Double
unit r = fromIntegral (r `mod` 1000000) / 1000000
sturm :: Double -> Double -> [Int]
sturm a rho = [ floor (fromIntegral (k+1)*a+rho) - floor (fromIntegral k*a+rho) | k <- [0::Int ..] ]
sim :: Int -> ([Int]->Bool) -> [Int] -> Bool
sim bts d t = go php0 mhp0 [] t bts where
  go p m h (b:r) l | m<=0 = True | p<=0 = False | l<=0 = False
                   | otherwise = let x = d h in go (if x&&b==1 then p-whiffD else p) (if x&&b==0 then m-hitD else m) (b:h) r (l-1)
  go _ _ _ [] _ = False
tab :: Int -> [Int] -> M.Map [Int] Int
tab n pre = M.map (\(z,o)->if z>=o then 0 else 1) (M.fromListWith (\(a,b)(c,d)->(a+c,b+d))
  [ (w, if nx==0 then (1,0) else (0,1)) | (w,nx) <- zip (map (take n) (takeWhile ((>=n).length) (iterate tail pre))) (drop n pre) ])
mem :: Int -> M.Map [Int] Int -> [Int] -> Bool
mem n tb h = length h>=n && M.findWithDefault 1 (reverse (take n h)) tb == 0
tapes :: [[Int]]
tapes = [ sturm (0.52 + 0.10*unit (lcg (fromIntegral s*104729))) (unit (lcg (fromIntegral s*7919))) | s <- [1..200::Int] ]
main :: IO ()
main = do
  putStrLn "  alpha in [0.52,0.62], 30 hits needed, certain-hit density ~0.14"
  putStrLn "  beats   certain hits available   after-HH    mem8"
  mapM_ row [60, 90, 120, 180, 240, 320, 420]
  where
    row b = do
      let pct f = printf "%5.0f%%" (100 * fromIntegral (length (filter id (map f tapes))) / 200 :: Double) :: String
      printf "  %5d %14.0f          %8s %7s\n" b (0.14 * fromIntegral b :: Double)
        (pct (sim b (\h -> take 2 h == [1,1])))
        (pct (\t -> sim b (mem 8 (tab 8 (take 4000 (drop 6000 t)))) t))
