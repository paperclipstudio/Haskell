import Data.Fixed (mod')
import Text.Printf ()
import Debug.Trace

-- Coord data type
data Position = Coor Float Float
origin = Coor 0 0
sumPos (Coor x y) = x + y
vect (Coor x y) = max (abs x) (abs y)
instance Show Position where
    show (Coor x y) = "(" ++ Prelude.show x ++ "," ++ Prelude.show y ++ ")"

--- every number N can be put in the form N = (S + K)**2 where S is a integer and 0 <= K < 1
--- where if k == 0 then N would be a square.
--- In this problem you can group numbers into L-Blocks by there S number, then there K value is
--- based on the angle pass 

getSNumber coor = (2 * vect coor ) + if (sumPos coor) > 0 then (-1.0) else 0.0  

-- Takes in S and percent to increase it by (between 0 and 2S)
-- as (S + 2S + 1) == (S+1) **
funK s n = (sqrt $ (s ** 2) + (2 * n * s)) - s

--atan' :: Position -> Double
--atan' input@(Coor x y) | trace ("aTan' has an issue with" ++ show (sumPos input) ++ " and " ++ show y) False = undefined
atan' input@(Coor x y)
    -- the results are mirrored on the x+y=0.5 line
    | x == (-y) && y < 0 = 999
    | sumPos input > 1e-15 = atan' (Coor (0.5 - x) (0.5 - y))
    | x == 0 = 0.75
    -- Forces 180 rotation to be 1 not 0
    | x == (-y) && y < 0 = 999
    | otherwise = result + if result < 0 then 1 else 0
    where 
        result = ((atan (y/x)) / pi) + 0.25
        
ans (Coor 0 1) = 3
ans origin = 0
ans input@(Coor x y) = 
    let 
        s = getSNumber input
        alpha = (pi/(2 * s)) * (y - x + s)
        mappedInput = squareToCircleMapping input
        k = funK s (atan' mappedInput)
    in
        round $ (s + k) ** 2 

-- grid of (x, y) coords to use as input
grid size = reverse $ [row y | y <- [(-size)..(size)]] where row y = [Coor x y| x <- [(0-size)..size]]

sectonGrid startX startY endX endY = [row y | y <- [startY.. endY]] where row y = [Coor x y| x <- [startX.. endX]]

mappedGrid = map2D ans (sectonGrid 90 (-100)  (100) (-90)) 

mapping2 origin = 0
mapping2 input@(Coor x y)
    | x >= 0 && y >= 0 = (angle * (  s + y - x)) / s -- Top Right
    | x >= 0 && y <  0 = (angle * (7*s + y + x)) / s -- Bottom Right
    | x <  0 && y >= 0 = (angle * (3*s - y - x)) / s -- Top Left
    | x <  0 && y <  0 = (angle * (5*s - y + x)) / s -- Bottom Left
    where 
        s = vect input
        angle = 45

-- Mappes from from points equaly spaced around a square of side N to equaly
-- Spaced around a circle of diameter N
squareToCircleMapping :: Position -> Position
squareToCircleMapping origin = origin
squareToCircleMapping input@(Coor x y) = 
    let
        angle = mapping2( input ) * (pi / 180)
        s = max (abs x) (abs y)
        newX = s * (cos angle)
        newY = s * (sin angle)
    in
        Coor newX newY
-- output 
grid2 = map2D ans (grid 3)
grid3 = map2D squareToCircleMapping (grid 5)

map2D func array =  map (map func) array

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