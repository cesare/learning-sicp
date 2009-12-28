--
-- 1.2.5 Greatest Common Divisors
--

ourGcd :: (Integral a) => a -> a -> a
ourGcd a 0 = a
ourGcd a b = ourGcd b (rem a b)
