--
-- 1.2.4 Exponentiation
--

expt :: (Num a) => a -> a -> a
expt b n = exptIter b n 1

exptIter :: (Num a) => a -> a -> a -> a
exptIter b 0 prod = prod
exptIter b counter prod = exptIter b (counter - 1) (b * prod)


square :: (Num a) => a -> a
square n = n * n

fastExpt :: (Integral a) => a -> a -> a
fastExpt _ 0 = 1
fastExpt b n | even n = square (fastExpt b (div n 2))
fastExpt b n = b * (fastExpt b (n - 1))

