module Board where
    import Tile
    type Coor = (Int, Int)

    data Board = Board {
    tiles :: [[Tile]],
    players :: [Coor],
    goal :: Coor
    }

    showOneLine :: [Tile] -> String
    showOneLine = foldr (\y x -> x ++ (show y)) ""

    instance Show Board where
        show (xs) = foldr (\x y -> showOneLine x ++ "\n" ++ y) "" (tiles xs)

    --testBoard :: Board 
    testBoard :: Board
    testBoard = (Board [[Corner, Straight, Corner], [Tee, Goal, Tee], [Corner, Straight, Corner]] [(0, 0)] (1, 1))
