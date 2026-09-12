module Main where

-- A tape with squares on BOTH sides of you. Exactly a Game of Life grid,
-- but one-dimensional.        past <- | you | -> future
data Zipper a = Zipper [a] a [a]

extract :: Zipper a -> a                 -- read the square you're standing on
extract (Zipper _ a _) = a

left, right :: Zipper a -> Zipper a      -- take one step
left  (Zipper (l:ls) a rs) = Zipper ls l (a:rs)
right (Zipper ls a (r:rs)) = Zipper (a:ls) r rs

instance Functor Zipper where
  fmap f (Zipper ls a rs) = Zipper (map f ls) (f a) (map f rs)

-- every square, replaced by "the whole tape as seen from that square"
duplicate :: Zipper a -> Zipper (Zipper a)
duplicate z = Zipper (tail (iterate left z)) z (tail (iterate right z))

-- THE DRAG. one rule, applied at every square.
extend :: (Zipper a -> b) -> Zipper a -> Zipper b
extend f = fmap f . duplicate

------------------------------------------------------------------
-- RULE ONE: Game of Life (1D, rule 90).  alive iff exactly one neighbour is.
------------------------------------------------------------------
life :: Zipper Bool -> Bool
life z = extract (left z) /= extract (right z)

start :: Zipper Bool
start = Zipper (repeat False) True (repeat False)

------------------------------------------------------------------
-- RULE TWO: the player.  Now he can see BACKWARDS too.
------------------------------------------------------------------
data Move = Strike | Block deriving (Eq, Show)

player :: Zipper Int -> Move
player z
  | extract (right z) >= 5                    = Block   -- I see the wind-up
  | extract (left z) >= 3 && extract z >= 3    = Block   -- he's pressuring me, cover up
  | otherwise                                  = Strike

fightTape :: Zipper Int
fightTape = Zipper (repeat 0) (head as) (tail as)
  where as = cycle [1,1,6,1,2,7,1,1,3,8]

------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "--- extend life : the drag, on cells ---"
  mapM_ (putStrLn . render) (take 16 (iterate (extend life) start))
  putStrLn ""
  putStrLn "--- extend player : the SAME drag, on moments ---"
  putStrLn "  last  now  next | move"
  mapM_ putStrLn (take 12 (walk (extend annotate fightTape)))
  where
    render (Zipper ls a rs) =
      [ if b then '#' else ' ' | b <- reverse (take 30 ls) ++ [a] ++ take 30 rs ]
    annotate z = pad (extract (left z)) ++ pad (extract z) ++ pad (extract (right z))
                 ++ "  | " ++ show (player z)
    pad n = let t = show n in replicate (6 - length t) ' ' ++ t
    walk z = extract z : walk (right z)
