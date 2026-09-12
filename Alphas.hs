-- Two Sturmian monsters with the same factor complexity and opposite characters.
module Main where
import qualified Data.Set as S
import Data.List (tails)
import Text.Printf (printf)

nTerms :: Int
nTerms = 20000

sturm :: Double -> [Int]
sturm a = [ floor (fromIntegral (k+1) * a) - floor (fromIntegral k * a) | k <- [0 .. nTerms-1] ]

phi :: Double
phi = (1 + sqrt 5) / 2

-- alpha as a continued fraction [0; a1, a2, ...] with a golden tail
cf :: [Double] -> Double
cf = foldr (\a acc -> 1 / (a + acc)) (1 / phi) 

alphaEven, alphaLumpy :: Double
alphaEven  = 1 / (phi * phi)     -- [0;2,1,1,1,...]  every partial quotient 1
alphaLumpy = cf [2, 8]           -- [0;2,8,1,1,1,...] one big partial quotient

complexity :: [Int] -> Int -> Int
complexity w n = S.size (S.fromList [ take n t | t <- take (length w - n) (tails w) ])

sym :: Int -> Char
sym 0 = '.'
sym _ = 'H'

main :: IO ()
main = do
  mapM_ report [("alpha = 1/phi^2  [0;2,1,1,1,...]", alphaEven)
               ,("alpha = [0;2,8,1,1,1,...]       ", alphaLumpy)]
  putStrLn "p(n) = n+1 for both, so both are Sturmian -- identical complexity, opposite feel."
  where
    report (nm, a) = do
      let w = sturm a
      printf "%s   density %.4f\n" nm a
      putStrLn ("   " ++ map sym (take 100 w))
      putStrLn ("   " ++ map sym (take 100 (drop 100 w)))
      printf "   p(n) for n=1..6 : %s\n\n" (show [ complexity w n | n <- [1..6] ])
