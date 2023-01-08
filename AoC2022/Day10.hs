module Main where

parse = (Nothing <$ string "noop") <|> (Just <$> (string "addx " *> posNegNum))

doStep x Nothing = (x, [x])
doStep x (Just y) = (x+y, [x, x + y])

solve = solve2 . (0:) . concat . snd . mapAccumL doStep 1

solve1  = sum . map biproduct . filter ((==20) . (`mod` 40) . fst) . zip [1..] 

solve2  = unlines . chunksOf 40 . zipWith getType [0 .. ]
        where
            getType cell p = if (cell `mod` 40) `elem` [p-1..p+1] then '#' else '.'

main = readFile "day10.txt" >>= (parseStr (parseLines parse) >>>
                        fmap solve >>> putStrLn . fromRight "Error")
