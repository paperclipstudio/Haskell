import Data.List
test :: [[Integer]]
test = 
    id
    $ filter ((==0).(`mod` 17).subDigits 8)
    $ filter ((==0).(`mod` 13).subDigits 7)
    $ filter ((==0).(`mod` 11).subDigits 6)
    $ filter ((==0).(`mod` 7).subDigits 5)
    $ filter ((==0).(`mod` 5).subDigits 4)
    $ filter ((==0).(`mod` 3).subDigits 3)
    $ filter ((==0).(`mod` 2).subDigits 2)
    $ permutations [0..9]

solve = sum (map arrayToInt test)

subDigits 1 (a:b:c:_) = 100 * a + 10 * b + c
subDigits n (x:xs) = subDigits (n-1) xs

arrayToInt :: [Integer] -> Integer
arrayToInt = foldl (\x y-> 10*x + y) 0