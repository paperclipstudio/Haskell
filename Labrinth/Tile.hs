module Tile where
    import Coor

    data Tile = BackTrack | Fire | Freeze | Double | Tee Coor | Straight Coor | Goal Coor| Corner Coor
        deriving (Eq)

    instance Show Tile where
        show (Tee _) = "T"
        show (Straight _) =
            "-"
        show (Corner _)= 
            "¬"
        show (Goal _)=
            "+"
        show _ = "A"