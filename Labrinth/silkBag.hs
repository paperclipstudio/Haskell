import Tile
import Data.Hashable

data SilkBag = SilkBag {
    tiles :: [Tile],
    seed :: Int
} deriving (Show)



isBagEmpty :: SilkBag -> Bool
isBagEmpty bag = tiles bag == []

draw :: SilkBag -> (SilkBag, Tile)
draw (SilkBag [] _ ) = error "Cannot Draw from Empty bag"
draw (SilkBag (x:xs) seed) = (newBag, newTile)
    where
        newBag = (SilkBag xs seed)
        newTile = x

insertTile :: SilkBag -> Tile -> SilkBag
insertTile (SilkBag [] seed) tile = (SilkBag [tile] seed)
insertTile (SilkBag tiles seed) tile = (SilkBag newTiles newSeed)
    where
        newTiles = take x tiles ++ tile : drop (x) tiles
        x = mod (hash seed) (length tiles)
        newSeed = hash seed

--testBag = SilkBag [] 2344
