
primes = 2 : [x | x <- [3..], all (\y -> x `mod` y /= 0) 
                   (takeWhile (<= (floor . sqrt $ fromIntegral x)) primes)]

divisors x = 
    map fst
    $ filter ((== 0).snd) 
    $ map (\(i,e) -> (i, x `mod` e))
    $ zip [1..x] [1..x]


isPrime x = x `elem` (takeWhile (<=x) primes)


-- Prime Generating Interger
isPGI x = all (\t -> isPrime(t + (x `div` t))) (divisors x)

solve = (sum.filter isPGI) [1..100000]
