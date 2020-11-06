module Board where
    import Tile
    type Coor = (Int, Int)

    data Board = Board {
    tiles :: [[Tile]],
    playersPieces :: [Coor],
    goal :: Coor
    }

    width = length.tiles
    height = length.(!! 0).tiles

    showOneLine :: [Tile] -> String
    showOneLine = foldr (\y x -> x ++ (show y)) ""

    instance Show Board where
        show (xs) = foldr (\x y -> showOneLine x ++ "\n" ++ y) "" (tiles xs)


    slideTile :: Board -> Tile -> Coor -> Board
    slideTile b t c 
        | fst c == -1 = b -- Left
        | fst c == width b = b -- right
        | snd c == 0 = b -- top
        | snd c == height b = b -- Bottom
        | otherwise = error $ "Invaild slide in location of " ++ show c

    right xss x y 
        | x == (length xss) - 1 = xss
        | otherwise = xss
    --testBoard :: Board 
    testBoard :: Board
    testBoard = (Board [[Corner, Straight, Corner], [Tee, Goal, Tee], [Corner, Straight, Corner]] [(0, 0)] (1, 1))
