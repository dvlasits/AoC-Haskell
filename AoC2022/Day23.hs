ops = [-1,0,1]
moves = [((-1, 0), (-1,) <$> ops), ((1, 0), (1,) <$> ops) , ((0, -1), (,-1) <$> ops) , ((0, 1), (,1) <$> ops)]
poses = concatMap snd moves

actOnProposals coorAndProps = map decidePos coorAndProps
                where
                    taken = M.fromListWith (+) $ map ((,1) . snd) coorAndProps
                    decidePos (prev, new) = if M.findWithDefault 0 new taken < 2  then new else prev

createProposals elfCoors moves = map (id &&& moveElf) elfCoors
                where
                    checkAllEmpty coor toMove  = not $ any ((`elem` elfCoors) . (+ coor)) toMove
                    moveElf coor | checkAllEmpty coor poses = coor
                                 | otherwise =  case find (checkAllEmpty coor . snd) moves of
                                                    Nothing -> coor
                                                    Just (new, _) -> coor + new

moveElves = actOnProposals .* createProposals

minMax = minimum &&& maximum
getBounds = (minMax . map fst) &&& (minMax . map snd)

main = do
    input <- lines <$> readFile "day23.txt"
    let elves = map fst $ input ^@.. itraversed <.> itraversed . filtered (=='#')
        moves' = map (take 4) . tails . cycle $ moves
        rounds = scanl' moveElves elves moves'
        ((yMin, yMax),(xMin, xMax)) = getBounds (rounds !! 10)
        firstStat =  findIndex (uncurry (==) . (each %~ sort)) . (zip <*> tail) $ rounds
    print . subtract (length elves) $ (yMax - yMin + 1) * (xMax - xMin + 1)
    print . fmap (+1) $ firstStat
