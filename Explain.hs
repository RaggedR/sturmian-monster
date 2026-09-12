-- extract and duplicate, printed.
module Main where

data Zipper a = Zipper [a] a [a]        -- past (nearest first) | here | future

extract :: Zipper a -> a                -- the square under your feet
extract (Zipper _ a _) = a

left, right :: Zipper a -> Zipper a     -- one step
left  (Zipper (l:ls) a rs) = Zipper ls l (a:rs)
right (Zipper ls a (r:rs)) = Zipper (a:ls) r rs

instance Functor Zipper where
  fmap f (Zipper ls a rs) = Zipper (map f ls) (f a) (map f rs)

duplicate :: Zipper a -> Zipper (Zipper a)     -- every square -> the whole tape from there
duplicate z = Zipper (tail (iterate left z)) z (tail (iterate right z))

extend :: (Zipper a -> b) -> Zipper a -> Zipper b
extend f = fmap f . duplicate

-- the Sturmian monster, with the cursor parked 20 blows in
sturmian :: [Int]
sturmian = take 200 (head (dropWhile ((< 200) . length) (iterate grow [0])))
  where grow = concatMap (\c -> if c == 0 then [0,1] else [0])

tape :: Zipper Int
tape = Zipper (reverse (take 20 sturmian)) (sturmian !! 20) (drop 21 sturmian)

-- rendering ---------------------------------------------------------------
sym :: Int -> String
sym 0 = " ."
sym _ = " H"

render :: Zipper Int -> String
render (Zipper ls a rs) =
  concatMap sym (reverse (take 4 ls)) ++ " [" ++ drop 1 (sym a) ++ "]" ++ concatMap sym (take 4 rs)

near :: Zipper a -> [a]                 -- the 9 entries around the cursor
near (Zipper ls a rs) = reverse (take 4 ls) ++ [a] ++ take 4 rs

main :: IO ()
main = do
  putStrLn "THE TAPE (cursor in brackets):"
  putStrLn ("   " ++ render tape)
  putStrLn ""
  putStrLn ("extract tape  =  " ++ show (extract tape) ++ "     -- one number. the blow you are standing on.")
  putStrLn ""
  putStrLn "duplicate tape  =  a tape whose every square holds A WHOLE TAPE:"
  putStrLn "   position | the tape as seen from THAT square"
  mapM_ (\(i, z) -> putStrLn ("   " ++ pad i ++ "   | " ++ render z))
        (zip [-4..4::Int] (near (duplicate tape)))
  putStrLn ""
  putStrLn "LAW 1:  extract (duplicate z)  ==  z"
  putStrLn ("   left  side: " ++ render (extract (duplicate tape)))
  putStrLn ("   right side: " ++ render tape)
  putStrLn ""
  putStrLn "LAW 2:  fmap extract (duplicate z)  ==  z"
  putStrLn ("   left  side:" ++ concatMap sym (near (fmap extract (duplicate tape))))
  putStrLn ("   right side:" ++ concatMap sym (near tape))
  putStrLn ""
  putStrLn "AND THAT IS WHY extend WORKS.  extend f = fmap f . duplicate :"
  putStrLn ("   tape      " ++ concatMap sym (near tape))
  putStrLn ("   next blow " ++ concatMap (\b -> if b then "  B" else "  A") (near (extend isHeavyNext tape)))
  where
    pad i = let t = show i in replicate (2 - length t) ' ' ++ t
    isHeavyNext z = extract (right z) == 1
