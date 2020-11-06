import Tile 
import Board
import SilkBag
data Turn = Int
data Phase = Draw | Move | Slide | Action 
data GameState = Turn
type Player = [Tile]

type SlideChoice = (Tile, Coor)
type ActionPlace = (Tile, Coor)
type MoveChoice = Coor

gameLogic :: GameState -> SlideChoice -> ActionPlace -> MoveChoice -> GameState
gameLogic gs sc ap mc = gs

main = putStr "Hello World"


