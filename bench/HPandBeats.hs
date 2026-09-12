module Main where
import qualified Data.Map.Strict as M
import Text.Printf (printf)

hitDmg, whiffDmg :: Int
hitDmg = 2; whiffDmg = 5

lcg :: Integer -> Integer
lcg x = (1103515245 * x + 12345) `mod` 2147483648
unit :: Integer -> Double
unit r = fromIntegral (r `mod` 1000000) / 1000000
sturmian :: Double -> Double -> [Int]
sturmian a rho = [ floor (fromIntegral (k+1)*a+rho) - floor (fromIntegral k*a+rho) | k <- [0::Int ..] ]

sim :: (Int, Int) -> Int -> ([Int] -> Bool) -> [Int] -> String
sim (php0, mhp0) bts decide tape = go php0 mhp0 [] tape bts
  where
    go php mhp hist (blow:rest) left
      | mhp <= 0  = "win"
      | php <= 0  = "die"
      | left <= 0 = "time"
      | otherwise = let hit = decide hist
                    in go (if hit && blow == 1 then php - whiffDmg else php)
                          (if hit && blow == 0 then mhp - hitDmg   else mhp)
                          (blow:hist) rest (left-1)
    go _ _ _ [] _ = "time"

tableN :: Int -> [Int] -> M.Map [Int] Int
tableN n pre = M.map (\(z,o) -> if z >= o then 0 else 1) (M.fromListWith plus
  [ (w, if nx == 0 then (1,0) else (0,1))
  | (w, nx) <- zip (map (take n) (takeWhile ((>=n) . length) (iterate tail pre))) (drop n pre) ])
  where plus (a,b) (c,d) = (a+c,b+d)

runs :: [([Int], [M.Map [Int] Int])]
runs = [ (t, [ tableN n (take 4000 (drop 6000 t)) | n <- [2,4,6,8,12] ])
       | s <- [1..300::Int]
       , let t = sturmian (0.52 + 0.10 * unit (lcg (fromIntegral s * 104729)))
                          (unit (lcg (fromIntegral s * 7919))) ]

pct :: (([Int], [M.Map [Int] Int]) -> String) -> String
pct f = printf "%4.0f%%" (100 * fromIntegral (length [()|r<-runs, f r == "win"]) / 300 :: Double)

mem :: Int -> M.Map [Int] Int -> [Int] -> Bool
mem n tb h = length h >= n && M.findWithDefault 1 (reverse (take n h)) tb == 0

perfect :: [Int] -> [Int] -> Bool
perfect tape h = tape !! length h == 0        -- sees the next blow: the ceiling

main :: IO ()
main = do
  printf "hit 2, whiff 5.  you 20 HP, monster 60 HP, 120 beats.  300 fights\n\n"
  putStrLn "  strategy            win%"
  printf "  mash                %s\n" (pct (\(t,_) -> sim (20,60) 120 (const True) t))
  printf "  after one strike    %s\n" (pct (\(t,_) -> sim (20,60) 120 (\h -> take 1 h == [1]) t))
  printf "  after HH (certain)  %s\n" (pct (\(t,_) -> sim (20,60) 120 (\h -> take 2 h == [1,1]) t))
  sequence_ [ printf "  memory %-2d           %s\n" n
                (pct (\(t,tbs) -> sim (20,60) 120 (mem n (tbs !! i)) t))
            | (i,n) <- zip [0..] [2,4,6,8,12::Int] ]
  printf "  PERFECT (ceiling)   %s\n" (pct (\(t,_) -> sim (20,60) 120 (perfect t) t))
