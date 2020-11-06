module Tile where

data Tile = BackTrack | Fire | Freeze | Double | Tee | Straight | Goal | Corner 
    deriving (Eq)

instance Show Tile where
    show Tee = "T"
    show Straight =
        "-"
    show Corner = 
        "¬"
    show Goal =
        "+"
    show _ = "A"