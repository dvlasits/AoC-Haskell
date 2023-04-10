arrToFunc :: [(Char, (Int, Int) -> (Int, Int))]
arrToFunc = [('^', _1 -~ 1), ('v', _1 +~ 1), ('>', _2 +~ 1), ('<', _2 -~ 1)]

calcBounds wind = ((maximum . map fst) &&& (maximum . map snd)) .  map fst $ filter ((=='#') . snd) wind

moveWinds :: (Int, Int) -> [((Int, Int), Char)] -> [((Int, Int), Char)]
moveWinds (yMax, xMax) = withinBounds . map adjustCoor
                    where
                        adjustCoor (coor, w) = ((fromJust $ lookup w arrToFunc) coor, w)
                        withinBounds = over (traversed . _1) (\(y,x) -> (y `mod` yMax, x `mod` xMax))

nextStates :: (Int, Int) -> [[(Int, Int)]] -> ((Int, Int), Int) -> [((Int, Int), Int)]
nextStates (yMax, xMax) windStates ((y, x), time) = (,time+1) <$> filter inBounds newPos
                    where
                        newPos = filter (not . flip elem (windStates !! time)) [(y+1, x), (y-1, x), (y, x+1), (y, x-1), (y, x)]
                        inBounds (y, x) = (y == yMax && x == (xMax - 1)) || (y == -1 && x == 0) || (y >= 0 && y < yMax && x >= 0 && x < xMax)

main = do
    input <- lines <$> readFile "day24.txt"
    let wind' = input ^@.. itraversed <.> itraversed <. filtered (/= '.')
        wind = over (traversed . _1 . each) (subtract 1) wind'
        (yMax, xMax) = calcBounds wind
        windStates = (map . map) fst $ iterate (moveWinds (yMax, xMax)) (filter ((/='#') . snd) wind)
        doBfs start end = bfs (nextStates (yMax, xMax) windStates) ((==end) . fst) start

        roundTrips = foldl' (last . fromJust .* doBfs) ((-1, 0), 0) [(yMax, xMax - 1), (-1, 0), (yMax, xMax - 1)]

    print roundTrips
