module Tile where

data Tile = ActionTile | FloorTile
    deriving Eq
data ActionTile = BackTrack | Fire | Freeze | Double 
data FloorTile = Tee | Straight | Goal | Corner 

instance Show FloorTile where
    show Tee = "T"
    show Straight =
        "-"
    show Corner = 
        "¬"
    show Goal =
        "+"