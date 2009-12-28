--
-- Exercise 1.18
--
double :: (Integral a) => a -> a
double n = n * 2

halve :: (Integral a) => a -> a
halve n = div n 2

multiply :: (Integral a) => a -> a -> a
multiply a b = multiplyIter a b 0

multiplyIter :: (Integral a) => a -> a -> a -> a
multiplyIter a b s
    | b == 0    = s
    | even b    = multiplyIter (double a) (halve b) s
    | otherwise = multiplyIter a (b - 1) (s + a)
