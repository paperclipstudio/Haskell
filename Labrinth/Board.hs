module Board where
    import Tile 
    import Coor

    data Board = Board {
    tiles :: [Square],
    playersPieces :: [Coor],
    goal :: Coor,
    width :: Int,
    height :: Int
    }


    instance Show Board where
        show (xs) = [Coor (x y) | x <- [0..width-1], y <- [0..height-1]]


    slideTile :: Board -> Tile -> Coor -> Board
    slideTile b t (Coor c) 
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
    testBoard = (Board [
        (Square Corner (Coor (0, 0))), (Square Corner (Coor (1, 0))), (Square Corner (Coor (2, 0))),
        (Square Corner (Coor (0, 1))),(Square Corner (Coor (1, 1))),(Square Corner (Coor (2, 1))),
        (Square Corner (Coor (0, 2))),(Square Corner (Coor (1, 2))),(Square Corner (Coor (2, 2)))] 
        [Coor (0, 0)] (Coor (1, 1)) 3 3)