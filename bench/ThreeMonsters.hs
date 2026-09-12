module Main where
import qualified Data.Map.Strict as M
import Text.Printf (printf)

php0, mhp0, beats, hitD, whiffD :: Int
php0 = 20; mhp0 = 60; beats = 120; hitD = 2; whiffD = 5

lcg :: Integer -> Integer
lcg x = (1103515245 * x + 12345) `mod` 2147483648
unit :: Integer -> Double
unit r = fromIntegral (r `mod` 1000000) / 1000000

sturm :: Double -> Double -> [Int]
sturm a rho = [ floor (fromIntegral (k+1)*a+rho) - floor (fromIntegral k*a+rho) | k <- [0::Int ..] ]

-- period 7, density 4/7 = 0.571; random with the same density
periodicW :: Int -> [Int]
periodicW ph = drop ph (cycle [1,1,0,1,0,1,0])
randomW :: Integer -> [Int]
randomW sd = map (\x -> if unit x < 0.571 then 1 else 0) (iterate lcg sd)

sim :: ([Int] -> Bool) -> [Int] -> Bool
sim decide tape = go php0 mhp0 [] tape beats
  where
    go php mhp hist (b:rest) left
      | mhp <= 0  = True
      | php <= 0  = False
      | left <= 0 = False
      | otherwise = let hit = decide hist
                    in go (if hit && b == 1 then php - whiffD else php)
                          (if hit && b == 0 then mhp - hitD  else mhp)
                          (b:hist) rest (left-1)
    go _ _ _ [] _ = False

tableN :: Int -> [Int] -> M.Map [Int] Int
tableN n pre = M.map (\(z,o) -> if z >= o then 0 else 1) (M.fromListWith plus
  [ (w, if nx == 0 then (1,0) else (0,1))
  | (w, nx) <- zip (map (take n) (takeWhile ((>=n) . length) (iterate tail pre))) (drop n pre) ])
  where plus (a,b) (c,d) = (a+c,b+d)

mem :: Int -> M.Map [Int] Int -> [Int] -> Bool
mem n tb h = length h >= n && M.findWithDefault 1 (reverse (take n h)) tb == 0

corpus :: String -> [[Int]]
corpus "sturmian" = [ sturm (0.52 + 0.10 * unit (lcg (fromIntegral s * 104729)))
                            (unit (lcg (fromIntegral s * 7919))) | s <- [1..300::Int] ]
corpus "periodic" = [ periodicW (s `mod` 7) | s <- [1..300::Int] ]
corpus _          = [ randomW (fromIntegral s * 7919 + 3) | s <- [1..300::Int] ]

rate :: String -> Int -> String
rate nm n = printf "%4.0f%%" (100 * fromIntegral (length (filter id
              [ sim (mem n (tableN n (take 4000 (drop 6000 t)))) t | t <- corpus nm ])) / 300 :: Double)

main :: IO ()
main = do
  printf "you %d HP, monster %d HP, %d beats. density ~0.57 in all three.  300 fights\n\n" php0 mhp0 beats
  putStrLn "  monster    mem2  mem4  mem6  mem8  mem12"
  mapM_ (\nm -> printf "  %-9s %5s %5s %5s %5s %6s\n" nm
                  (rate nm 2) (rate nm 4) (rate nm 6) (rate nm 8) (rate nm 12))
        ["periodic", "sturmian", "random"]
