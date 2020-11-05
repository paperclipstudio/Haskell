import Tile
import Data.Hashable

data SilkBag = SilkBag {
    tiles :: [Tile],
    seed :: Int
}

isBagEmpty :: SilkBag -> Bool
isBagEmpty bag = tiles bag == []

draw :: SilkBag -> (SilkBag, Tile)
draw (SilkBag (x:xs) seed) = (newBag, newTile)
    where
        newBag = (SilkBag xs seed)
        newTile = x

insertTile :: SilkBag -> Tile -> SilkBag
insertTile (SilkBag tiles seed) tile = (SilkBag newTiles newSeed)
    where
        newTiles = take x tiles ++ tile : drop (x+1) tiles
        x = mod (hash seed) (length tiles)
        newSeed = hash seed

testBag = SilkBag [] 2344
