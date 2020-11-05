data Turn = Int
data Phase = Draw | Move | Slide | Action 
data GameState = Turn
data Board = Board {
    tiles :: [[FloorTile]],
    players :: [Coor],
    goal :: Coor
    }
type Player = [Tile]
data Tile = ActionTile | FloorTile
data ActionTile = BackTrack | Fire | Freeze | Double 
data FloorTile = Tee | Straight | Goal | Corner 


type SlideChoice = (Tile, Coor)
type ActionPlace = (Tile, Coor)
type MoveChoice = Coor

type Coor = (Int, Int)

type SilkBag = [Tile]

gameLogic :: GameState -> SlideChoice -> ActionPlace -> MoveChoice -> GameState
gameLogic gs sc ap mc = gs


showOneLine :: [FloorTile] -> String -> String
showOneLine = foldr (\y x -> x ++ (show y)) ""

instance Show Board where
    show (xs) = foldr (showOneLine) "" (tiles xs)

instance Show FloorTile where
    show Tee = "T"
    show Straight =
        "-"
    show Corner = 
        "¬"
    show Goal =
        "+"