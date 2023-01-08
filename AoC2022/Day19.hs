module Main where

ints2 :: Parser [Int]
ints2 = optional nonNumStuff *> many1 (integer <* nonNumStuff)

type Metals = [Int] -- 4 element array for metals
type Robots = [Int]
type Ix = Int

buyStuff :: (Metals, Robots) -> [(Ix, Metals)] -> [((Ix, (Metals, Robots)), Bool)]
buyStuff (metals, robots) toBuy = ((-1, (metals, robots)), True) : ((, False) <$> [(index, (zipWith subtract vals metals, robots & ix index %~ (+1)) )
                                        | (index,vals) <- toBuy, and $ zipWith (<=) vals metals])

idR :: Robots -> Robots
idR = id

solve1 :: [Int] -> Int
solve1 [ore, clay, ob1, ob2, ge1, ge2] = findBest 1 False [0,0,0,0] ([0,0,0,0],[1,0,0,0])
            where
                prices = [[ore, 0, 0, 0], [clay,0,0,0], [ob1, ob2, 0, 0], [ge1, 0, ge2, 0]]
                toBuy = zip [0,1..] prices
                findBest :: Int -> Bool -> Metals -> (Metals, Robots) -> Int
                findBest 33 _ mp (metals, _) = last metals
                findBest time skipped metalsPrev (metals, robots) = maximum . (0:) $ do
                                                                ((index, (m, r)), basic) <- filter (and . zipWith (>=) [4, 20, 20] . snd . snd . fst) (buyStuff (metals, robots) toBuy) 
                                                                guard (not (skipped && (index >= 0) && and (zipWith (<=) (prices !! index) metalsPrev)))
                                                                return $ findBest (time + 1) basic metals (zipWith (+) robots m, r)

solve (id:xs) = traceShowId $ solve1 xs


main = readFile "day19.txt" >>= (lines >>> traverse (parseStr ints2) >>> fmap (take 3) >>>
                        (fmap . fmap) solve >>> fmap product >>> print)
