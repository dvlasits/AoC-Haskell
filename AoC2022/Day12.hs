type Coor = (Int, Int)

nextStates :: V.Vector (V.Vector Char) -> Coor -> [Coor]
nextStates mat (x, y) =  mapMaybe check lookAt
        where
            lookAt = [(x + xm, y + ym) | (xm, ym) <- [(-1, 0), (1, 0), (0, 1), (0, -1)]]
            check (x', y') = do
                        orig <- mat ^? ix x  . ix y
                        new  <- mat ^? ix x' . ix y' 
                        let orig' = if orig == 'S' then 'a' else orig
                        let new'  = if new  == 'E' then 'z' else new
                        guard $ (ord new'- ord orig') <= 1
                        return (x', y')

findLetter :: V.Vector (V.Vector Char) -> Char -> [Coor]
findLetter mat toFind = fmap fst $ mat ^@.. itraversed <.> itraversed . filtered (==toFind)

main = do
    input <- V.fromList . fmap V.fromList . lines <$> readFile "day12.txt"
    let start = head $ findLetter input 'S'
        found = (==) . head $ findLetter input 'E'
        otherStarts = findLetter input 'a'
    print . fmap length $ bfs (nextStates input) found start
    print . minimum . fmap length . mapMaybe (bfs (nextStates input) found) $ otherStarts
