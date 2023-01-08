module Main where


score = (+) <$> (subtract 96 . ord) <*> (bool 0 58 . isUpper)

solve1 = map $ score . head . uncurry intersect . (splitAt =<< ((`div` 2) . length))

solve2 = map (score . head . foldl1' intersect) . chunksOf 3

main = readFile "Day3.txt" >>= (lines >>> solve2 >>> sum >>> print)
