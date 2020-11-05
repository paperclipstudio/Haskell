import Tile ( FloorTile(..), Tile )

data Turn = Int
data Phase = Draw | Move | Slide | Action 
data GameState = Turn
data Board = Board {
    tiles :: [[FloorTile]],
    players :: [Coor],
    goal :: Coor
    }
type Player = [Tile]



type SlideChoice = (Tile, Coor)
type ActionPlace = (Tile, Coor)
type MoveChoice = Coor

type Coor = (Int, Int)



gameLogic :: GameState -> SlideChoice -> ActionPlace -> MoveChoice -> GameState
gameLogic gs sc ap mc = gs


showOneLine :: [FloorTile] -> String
showOneLine = foldr (\y x -> x ++ (show y)) ""

instance Show Board where
    show (xs) = foldr (\x y -> showOneLine x ++ "\n" ++ y) "" (tiles xs)

--testBoard :: Board 
testBoard :: Board
testBoard = (Board [[Corner, Straight, Corner], [Tee, Goal, Tee], [Corner, Straight, Corner]] [(0, 0)] (1, 1))
