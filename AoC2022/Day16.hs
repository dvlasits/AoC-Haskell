type Parser = Parsec Void Text

parseLine :: Parser (String, Int, [String])
parseLine = do
    _ <- string "Valve "
    valveName <- many upperChar
    _ <- string " has flow rate="
    flowRate <- L.decimal
    _ <- takeWhile1P Nothing (not . isUpper)
    valves <- many upperChar `sepBy` string ", "
    return (valveName, flowRate, valves)

lookup' = fromJust .* flip lookup

createInfo :: [(String, Int, [String])] -> ([(String, Int)], Map (String, String) Int)
createInfo data' = (flowRates, distanceMap)
            where
                flowRates = filter ((>0) . snd) . map (view _1 &&& view _2) $ data'
                adjacencyMatrix = map (view _1 &&& view _3) data'
                pathPairs = [(x,y) | (x,_) <- ("AA", 0):flowRates, (y,_) <- ("AA",0):flowRates, x /= y, x < y]
                distances = [id, over _1 swap] <*> (zip <*> map (\(start, end) -> length . fromJust $ bfs (lookup' adjacencyMatrix) (==end) start)) pathPairs
                distanceMap = M.fromList distances

tryOptions :: Int -> [(String, Int)] -> String -> Map (String, String) Int -> Int
tryOptions time valvesLeft position distanceMatrix
                        | time == 0 = 0
                        | time < 0 = -100000000000
                        | otherwise = maximum . (0:) $ do
                                        (valve, flow) <- valvesLeft
                                        let toTravel = distanceMatrix ^?! ix (position, valve)
                                            best = tryOptions (time - toTravel - 1) (delete (valve, flow) valvesLeft) valve distanceMatrix
                                        return $ (flow * (time - toTravel - 1)) + best

main = do
    input <- T.lines <$> readFile "day16.txt"
    let Right parsed = traverse (parse parseLine "") input
        (flowRates, distances) = createInfo parsed
        subSets = filterM (const [True, False]) flowRates
        pairedSets = filter (uncurry (<)) $ map (\x -> (x, filter (`notElem` x) flowRates)) subSets
    print . maximum $ map (sumOf (each . to (\x -> tryOptions 26 x "AA" distances))) pairedSets
