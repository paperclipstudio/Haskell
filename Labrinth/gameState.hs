import Tile 
import Board
import SilkBag
data Turn = Int
data Phase = Draw | Move | Slide | Action 
    deriving (Show)
data GameState = GameState {
    playerTurn :: Int,
    numberOfPlayers :: Int,
    board :: Board,
    silkBag :: SilkBag,
    players :: [Player],
    phase :: Phase
} deriving (Show)
type Player = [Tile]

type SlideChoice = (Tile, Coor)
type ActionPlace = (Tile, Coor)
type MoveChoice = Coor

gameLogic :: GameState -> SlideChoice -> ActionPlace -> MoveChoice -> GameState
gameLogic gs sc ap mc = gs

drawPhase :: GameState -> GameState
drawPhase gs = newGs{silkBag = newBag, phase = Slide}
    where
        newGs = updateCurrentPlayer gs newPlayer 
        newPlayer = newTile:currentPlayer gs 
        (newBag, newTile) = draw $ silkBag gs

main = do
    putStrLn $ show testGameState
    putStrLn $ show $ drawPhase testGameState

currentPlayer gs = players gs !! playerTurn gs
updateCurrentPlayer gs player = gs {players = newPs}
    where 
        ps = players gs
        newPs = take turn ps ++ player : drop (turn+1) ps
        turn = playerTurn gs

floorPhase :: GameState -> SlideChoice -> GameState
floorPhase gs sc = gs{board = slideTile b t coor }
    where
        b = board gs
        t = fst sc
        coor = snd sc
testGameState :: GameState
testGameState = GameState 2 4 testBoard (SilkBag [Corner, Tee, Fire] 123) [[],[],[],[]] Draw