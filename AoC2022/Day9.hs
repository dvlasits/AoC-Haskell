notTouching c1 c2 = uncurry (||) . both (>1) . abs $ (c1 - c2)

updateTail t h = if notTouching t h then t + signum (h - t) else t

solve :: Int -> [Coor] -> Int
solve t = length . nubOrd . (!! t) . iterate (scanl updateTail 0) . scanl (+) 0

parsed :: [String] -> [Coor]
parsed [direc, amount] = replicate (read amount) $  case direc of 
                         "L" -> (-1, 0); "R" -> (1, 0); 
                         "U" -> (0, 1);  "D" -> (0, -1)

main = do
    contents <- readFile "day9.txt"
    let 
        preSol = concatMap (parsed . words) . lines $ contents
        solution = solve 9 preSol 
        in 
            print solution
