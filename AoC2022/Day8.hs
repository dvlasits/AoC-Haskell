module Main where

indmap f = zipWith f [0..]

appendInf [] = []
appendInf xs = init xs ++ [10]

oneSet (y, x, m) = biList . (reverse *** tail) . splitAt x $ (m !! y)

solve :: (Int -> [[Int]] -> a) -> ([a] -> Int) -> [[Int]]  -> Int
solve calcagg agg m  = agg . concat $ indmap (indmap . calcCoord) m
        where
            calcCoord y x val = calcagg val $ concatMap oneSet [(y,x,m), (x,y,m')]
            m' = transpose m

solve1 = solve (\v -> any (null . dropWhile (<v))) (length . filter id)
solve2 = solve (\v -> product . map (sum . fmap (+1) . findIndex (v<=) . appendInf)) maximum
            
main = readFile "day8.txt" >>= (lines >>> (fmap . fmap) (read . flip (:) []) >>> 
                                solve2 >>> print)
