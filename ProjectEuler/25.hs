fibb = 0:1:[(fibb !! x) + (fibb !! (x-1))| x<- [1..]]
solve = head $ dropWhile ((<1000).length.show.snd) $ zip [0..] fibb