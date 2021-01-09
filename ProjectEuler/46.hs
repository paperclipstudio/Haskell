primes = 2 : [x | x <- [3..], all (\y -> x `mod` y /= 0) 
    (takeWhile (<= (floor . sqrt $ fromIntegral x)) primes)]

test x = map (\y->sqrt((x-y) `div` 2)) $ takeWhile (<= x) primes
