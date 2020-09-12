import Data.Fixed (mod')
import Text.Printf ()

--- every number N can be put in the form N = (S + K)**2 where S is a integer and 0 <= K < 1
--- where if k == 0 then N would be a square.
--- In this problem you can group numbers into L-Blocks by there S number, then there K value is
--- based on the angle pass 

lBlock x y = (2 * max (abs x) (abs y) ) + if (x+y) > 0 then (-1.0) else 0.0  
-- Takes in S and percent to increase it by (between 0 and 2S)
-- as (S + 2S + 1) == (S+1) **
funK s n = (sqrt $ (s ** 2) + (2 * n * s)) - s

atan' :: (Ord p, Floating p) => p -> p -> p
atan' x y
    -- the results are mirrored on the x+y=0.5 line
    | (x + y) > 1e-5 = atan' (0.5 - x) (0.5 - y)
    | x == 0 = 0.75
    -- Forces 180 rotation to be 1 not 0
    | x == (-y) && y < x = 1
    | otherwise = result + if result < 0 then 1 else 0
    where 
        result = ((atan (y/x)) / pi) + 0.25
        
ans x y = 
    let 
        s = lBlock x y
        alpha = (pi/(2 * s)) * (y - x + s)
        (newX, newY) = mapping3(x, y)
        k = funK s (atan' newX newY)
    in
        round $ (s + k) ** 2 

split (x, y) = let
    (newX, newY) = mapping3(x, y)
    in
    atan' newX newY

-- grid of (x, y) coords to use as input
grid size = reverse $ [row y | y <- [(-size)..(size)]] where row y = [(x,y)| x <- [(-size)..size]]

sectonGrid = [row y | y <- [(-100.0)..(-90)]] where row y = [(x,y)| x <- [(-100.0)..(-90)]]

mappedGrid = applyFunc (\(x, y) -> ans x y) sectonGrid

mapping (x, y) = let s = max (abs x) (abs y)
                     alpha = x - y + 10
                     in
                         (((sin alpha) * s), (s * cos alpha))
mapping2(0, 0) = 0
mapping2 (x, y)
    | x >= 0 && y >= 0 = (angle * (  s + y - x)) / s -- Top Right
    | x >= 0 && y <  0 = (angle * (7*s + y + x)) / s-- Bottom Right
    | x <  0 && y >= 0 = (angle * (3*s - y - x)) / s     -- Top Left
    | x <  0 && y <  0 = (angle * (5*s - y + x)) / s-- Bottom Left
    where 
        s = max (abs x) (abs y)
        angle = 45

mapping3 (x, y) = 
    let
        angle = mapping2(x, y) * (pi / 180)
        s = max (abs x) (abs y)
        newX = s * (cos angle)
        newY = s * (sin angle)
    in
        (newX, newY)
-- output 
grid2 = applyFunc (\(x, y) -> ans x y) (grid 3)
grid3 = applyFunc mapping3 (grid 5)

applyFunc func array =  map (map func ) array
-- Code to pretty print out
printGrid :: Show a => [[a]] -> IO ()
printGrid input = do putStr.replace.(flip showList) "" $ map (map ((lpad 5).show)) input 

lpad :: Int -> [Char] -> [Char]
lpad m xs = replicate (m - length ys) ' ' ++ ys
    where ys = take m xs

replace list@(x:xs)  
    | xs == [] = ['\n']
    | take 3 list == [']',',','['] = '\n':(replace (drop 3 xs))
    | x == '"' = replace xs
    | x == '[' = replace xs
    | x == ']' = replace xs
    | otherwise = (x):(replace $ xs)