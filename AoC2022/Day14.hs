type Parser = Parsec Void Text

parseCoor :: Parser Coor
parseCoor = (,) <$> L.decimal <*> (single ',' *> L.decimal)

parseLine :: Parser [Coor]
parseLine = parseCoor `sepBy` string " -> "

constructSet :: [[Coor]] -> Set Coor
constructSet = S.fromList . concatMap fillIn
        where
            fillIn [start,end] = 
                    [start + ((i,i) * signum (end - start)) | i <- [0..uncurry max $ abs (end - start)]]

options (x, y) = [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]

findRest :: Int -> Coor -> Set Coor -> Maybe ((), Set Coor)
findRest lowest (x ,y) set
            | y == (lowest + 1) = Just ((), S.insert (x,y) set) -- | y > lowest = Nothing
            | otherwise  = case filter (not . (S.member ?? set)) (options (x, y)) of
                            [] -> if (x,y) == (500,0) then Nothing else Just ((), S.insert (x, y) set)
                            (new:_) -> findRest lowest new set

main = do
    input <- readFile "day14.txt"
    let Right parsed = parse (parseLine `sepBy` newline) "" input
        mySet = constructSet $ concatMap (divvy 2 1) parsed
        Just lowest = maximumOf (folded . _2) mySet
    print . length . unfoldr (findRest lowest (500, 0)) $ mySet
