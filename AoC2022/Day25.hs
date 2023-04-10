toDec = [('0', 0), ('1', 1), ('2', 2), ('-', -1), ('=', -2)]

toSnaf = map swap toDec

lookup' :: Eq a => a -> [(a, b)] -> b
lookup' = fromJust .* lookup

snafToDec = sum . zipWith (*) (iterate (*5) 1) . reverse


base5toSnaf dec = reverse . go . reverse . map digitToInt $ showIntAtBase 5 intToDigit dec ""
            where
                go [x] | x < 3 = [intToDigit x]
                       | otherwise = [lookup' (x - 5) toSnaf, '1']
                go (x:y:xs) | x < 3 = intToDigit x : go (y:xs)
                            | otherwise = lookup' (x - 5) toSnaf : go (y+1:xs)


main = do
    input <- lines <$> readFile "day25.txt"
    let snafu = (map . map) (`lookup'` toDec) input
        summed = sum $ map snafToDec snafu
    print summed
    print . base5toSnaf $ summed
