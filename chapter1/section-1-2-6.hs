--
-- Testing for Primality
--

square :: (Integral a) => a -> a
square n = n * n

expmod :: (Integral a) => a -> a -> a -> a
expmod base ex m
    | ex == 0   = 1
    | even ex   = rem (square (expmod base (div ex 2) m)) m
    | otherwise = rem (base * (expmod base (ex -1) m)) m

