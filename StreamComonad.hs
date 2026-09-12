-- The comonad on Stream, printed.
module Main where

import Text.Printf (printf)

data Stream a = a :> Stream a
infixr 5 :>

instance Functor Stream where
  fmap f (a :> s) = f a :> fmap f s

extract :: Stream a -> a                      -- the head
extract (a :> _) = a

tailS :: Stream a -> Stream a
tailS (_ :> s) = s

duplicate :: Stream a -> Stream (Stream a)    -- every tail = every starting point
duplicate s = s :> duplicate (tailS s)

extend :: (Stream a -> b) -> Stream a -> Stream b
extend f = fmap f . duplicate

fromList :: [a] -> Stream a
fromList (x:xs) = x :> fromList xs
fromList []     = error "streams do not end"

takeS :: Int -> Stream a -> [a]
takeS 0 _        = []
takeS n (a :> s) = a : takeS (n-1) s

shift :: Int -> Stream a -> Stream a
shift n s = iterate tailS s !! n

-- the stream 0,1,2,3,... : the value at each position IS the position
nats :: Stream Int
nats = fromList [0..]

row :: String -> [Int] -> String
row lbl xs = lbl ++ concatMap (printf "%4d") xs

main :: IO ()
main = do
  putStrLn "s = 0 1 2 3 4 5 ...    (value = position, so shifts are visible)\n"

  putStrLn "duplicate s  --  row i is the stream starting at position i:"
  mapM_ (\(i, t) -> putStrLn (row (printf "  row %d : " (i::Int)) (takeS 8 t)))
        (zip [0..] (takeS 6 (duplicate nats)))
  putStrLn "\n  ^ that is the ADDITION TABLE.  entry (i,j) = i + j."
  putStrLn "    duplicate shifts the start, and shifting by i then j = shifting by i+j.\n"

  putStrLn "LAW 1   extract (duplicate s) == s        -- the 0th tail is s itself.  (0 + n = n)"
  putStrLn (row "  extract (duplicate s) : " (takeS 8 (extract (duplicate nats))))
  putStrLn (row "  s                     : " (takeS 8 nats))
  putStrLn ""

  putStrLn "LAW 2   fmap extract (duplicate s) == s   -- the heads of the tails.  (m + 0 = m)"
  putStrLn (row "  fmap extract (dup s)  : " (takeS 8 (fmap extract (duplicate nats))))
  putStrLn (row "  s                     : " (takeS 8 nats))
  putStrLn ""

  putStrLn "LAW 3   duplicate (duplicate s) == fmap duplicate (duplicate s)"
  putStrLn "        -- shift i, then j, then k.  both sides land at i+j+k.  ((m+n)+p = m+(n+p))"
  let lhs = duplicate (duplicate nats)
      rhs = fmap duplicate (duplicate nats)
      at t i j k = extract (shift k (extract (shift j (extract (shift i t)))))
  printf "  entry (2,3,1):  left %d   right %d   (2+3+1 = 6)\n"
         (at lhs 2 3 1) (at rhs 2 3 1)
  printf "  entry (0,4,2):  left %d   right %d   (0+4+2 = 6)\n"
         (at lhs 0 4 2) (at rhs 0 4 2)
  putStrLn ""

  putStrLn "WHAT extend IS FOR: a rule that needs to see ahead, run at every position."
  let biggestOfNext3 s = maximum (takeS 3 s)
      bumpy = fromList (cycle [3,1,4,1,5,9,2,6])
  putStrLn (row "  stream                : " (takeS 12 bumpy))
  putStrLn (row "  extend (max of next 3): " (takeS 12 (extend biggestOfNext3 bumpy)))
  putStrLn "\n  fmap could not do this: it only ever hands the rule ONE number."
