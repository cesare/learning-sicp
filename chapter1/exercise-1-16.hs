--
-- Exercise 1.16
--
xpt :: (Integral a) => a -> a -> a
xpt b n = xptIter b n 1

xptIter :: (Integral a) => a -> a -> a -> a
xptIter b n a
    | n == 0    = a
    | even n    = xptIter (b * b) (div n 2) a
    | otherwise = xptIter b (n - 1) (a * b)
