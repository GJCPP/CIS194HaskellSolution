module Fibonacci where 

-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(x, y) -> (y, x + y)) (0, 1)

-- Exercise 3
data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream a tail) = a : streamToList tail

instance Show a => Show (Stream a) where
    show stream = show10 stream 0 where
        show10 (Stream val tail) i
            | i >= 9    = show val
            | otherwise = show val ++ " " ++ show10 tail (i + 1)

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat val = Stream val (streamRepeat val)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream val tail) = Stream (f val) $ streamMap f tail

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Stream (f s) $ streamFromSeed f (f s)

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x tx) ty = Stream x (interleaveStreams ty tx)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+1) ruler)

-- Exercise 6
x :: Stream Integer
x = Stream 0 (Stream 1 (streamRepeat 0))

instance Num (Stream Integer) where
    fromInteger x = Stream x $ streamRepeat 0
    (+) (Stream x tx) (Stream y ty) = Stream (x + y) $ tx + ty
    negate (Stream x tx) = Stream (-x) (-tx)
    (*) (Stream a ta) sb@(Stream b tb) = Stream (a*b) $ ta * sb + streamMap (*a) tb
    
instance Fractional (Stream Integer) where
    (/) (Stream x tx) (Stream y ty) = q
        where q = Stream (x `quot` y) $ streamMap (`quot` y) (tx - q * ty)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer
instance Num Matrix where
    (*) (Matrix a b c d) (Matrix w x y z) = Matrix x1 x2 x3 x4
        where
            x1 = a * w + b * y
            x2 = a * x + b * z
            x3 = c * w + d * y
            x4 = c * x + d * z

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = fetch $ Matrix 0 1 1 1 ^ n
    where fetch (Matrix _ x _ _) = x
