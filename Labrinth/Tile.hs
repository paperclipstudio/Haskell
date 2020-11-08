module Tile where
    import Coor

    data Tile = BackTrack | Fire | Freeze | Double | Tee | Straight | Goal | Corner 
        deriving (Eq)

    instance Show Tile where
        show (Tee) = "T"
        show (Straight) =
            "-"
        show (Corner)= 
            "¬"
        show (Goal)=
            "+"
        show _ = "A"

    data Square = Square Tile Coor
    getCoor :: Square -> Coor
    getCoor (Square _ c) = c 
    instance Show Square where
        show (Square t _) = show t