import Data.List

primes = 2 : [x | x <- [3..], all (\y -> x `mod` y /= 0) 
    (takeWhile (<= (floor . sqrt $ fromIntegral x)) primes)]


primePermutations :: [Int] -> [Int]
primePermutations = (filter isPrime.map arrayToInt.reverse.sort.permutations)
solve = [(show (primePermutations [1..y])) ++ (show y)| y <- (reverse [1..9])]

isPrime x = 
    (all (/= 0).
    map (mod x).
    takeWhile (<= ((round.sqrt.fromIntegral) x))
    ) primes

arrayToInt :: (Num a) => [a] -> a
arrayToInt = foldl (\x y-> 10*x + y) 0
