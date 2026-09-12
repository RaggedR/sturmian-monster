-- Two monsters. The SAME numbers, asked two different questions.
--
--   a list   asks "WHICH fight?"   -- it branches, and it ends
--   a stream asks "WHEN in the fight?" -- it doesn't branch, and it never ends
module Main where

data World = World { grug :: Int, snel :: Int }

instance Show World where
  show (World g s) = "Grug" ++ bar g ++ " | Snel" ++ bar s
    where bar n = pad (show n) ++ " " ++ replicate (max 0 n) '#'
          pad t = replicate (3 - length t) ' ' ++ t

w0 :: World
w0 = World 7 9

------------------------------------------------------------------
-- PART 1.  [a] is a MONAD.  It holds ALTERNATIVES.
------------------------------------------------------------------
-- One round, where nobody knows how hard anyone hits.

oneRound :: World -> [World]
oneRound (World g s) = do
  hitOnSnel <- [1,2,3]       -- branch: three ways this could go
  hitOnGrug <- [1,2,3]       -- branch again: nine ways in total
  pure (World (g - hitOnGrug) (s - hitOnSnel))

-- >>= means "for each possibility, consider every possibility".
-- Note there is NO time here.  All nine of these are round one.

------------------------------------------------------------------
-- PART 2.  Stream a is a COMONAD.  It holds a POSITION IN TIME.
------------------------------------------------------------------

data Stream a = a :> Stream a       -- look: no nil. it cannot run out.
infixr 5 :>

extract :: Stream a -> a            -- always succeeds. `head []` does not.
extract (a :> _) = a

next :: Stream a -> Stream a
next (_ :> s) = s

duplicate :: Stream a -> Stream (Stream a)   -- every "from here onwards"
duplicate s = s :> duplicate (next s)

instance Functor Stream where
  fmap f (a :> s) = f a :> fmap f s

-- fmap  :: (       a -> b) -> Stream a -> Stream b   -- sees ONE moment
-- extend:: (Stream a -> b) -> Stream a -> Stream b   -- sees a moment AND its future
extend :: (Stream a -> b) -> Stream a -> Stream b
extend f = fmap f . duplicate

takeS :: Int -> Stream a -> [a]
takeS 0 _         = []
takeS n (a :> s)  = a : takeS (n-1) s

-- This time the dice are fixed so we can read it: Grug hits for 2, Snel for 1.
step :: World -> World
step (World g s)
  | g <= 0 || s <= 0 = World (max 0 g) (max 0 s)  -- the dead stay dead...
  | otherwise        = World (max 0 (g - 1)) (max 0 (s - 2))  -- ...stream runs on anyway

fight :: Stream World
fight = go w0 where go w = w :> go (step w)

------------------------------------------------------------------
-- The thing you can only do with a comonad.
------------------------------------------------------------------
-- A question NO single World can answer: "is Snel doomed within 3 ticks?"
-- It needs the moment *and what follows it*.  So its type is Stream -> b.
doomed :: Stream World -> Bool
doomed s = any ((<= 0) . snel) (takeS 4 s)

-- `extend doomed` asks that question at EVERY moment of the fight at once,
-- giving back a whole new stream: a prophetic health bar.
prophecy :: Stream Bool
prophecy = extend doomed fight

main :: IO ()
main = do
  putStrLn "=== LIST: which fight? (branching, finite) ==========="
  putStrLn ("after 1 round there are " ++ show (length (oneRound w0)) ++ " possible worlds:")
  mapM_ (putStrLn . ("   " ++) . show) (oneRound w0)
  putStrLn ("after 2 rounds: " ++ show (length (oneRound w0 >>= oneRound))
            ++ " possible worlds -- and the list ENDS. you can count it.")
  putStrLn ""
  putStrLn "=== STREAM: when in the fight? (one line, endless) ==="
  putStrLn "tick | world                      | Snel doomed within 3?"
  mapM_ putStrLn
    [ pad (show t) ++ "   | " ++ show w ++ "   | " ++ (if d then "YES" else "-")
    | (t, w, d) <- zip3 [0::Int ..] (takeS 12 fight) (takeS 12 prophecy) ]
  putStrLn "...and on, and on. there is no last element to reach."
  where pad t = replicate (4 - length t) ' ' ++ t
