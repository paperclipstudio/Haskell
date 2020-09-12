import System.Random

digits = [1..9]
--nums = [(x,y,z) | x <- digits, y <- digits, z <- digits, 7 == (abs (x - y)), 7 == abs (y - z)]

--addDigit :: Integer a => a -> a -> a -> [a]
addDigit x k = [x + newDigit | newDigit <- digits, (abs (newDigit - lastDigit)) == k]
    where lastDigit = mod x 10

--addDigit2 :: a -> a -> [a]
addDigit2 k x = (if lowerDigit >= 0 then [leftShift + lowerDigit] else []) ++ (if higherDigit <= 9 then [leftShift + higherDigit] else []) 
    where leftShift = (x * 10)
          lowerDigit = lastDigit - k
          higherDigit = lastDigit + k
          lastDigit = mod x 10


 
nums :: Integer -> Integer -> [Integer]
nums _ 1 = digits
nums k n = concat.map (addDigit2 k) $ nums k $ n - 1
--nums k n = (nums k 1) (nums k (n-1))


data Brick = Vertical | Angle | Flat | Cemented deriving (Eq, Show)

blocks :: [Brick]
blocks = replicate  100 Vertical
bIocks :: [Brick]
bIocks = replicate 42 Vertical ++ [Cemented] ++ replicate 57 Vertical

push :: [Brick] -> [Brick]
push (x:[]) = Flat:[]
push (Cemented:xs) = (Cemented:xs)
push (Vertical: xs) = if (head restOfBricks) == Flat then (Flat:restOfBricks) else Angle:(restOfBricks)
    where restOfBricks = push xs

main :: IO ()
main = do
    putStrLn $ show $ push bIocks

