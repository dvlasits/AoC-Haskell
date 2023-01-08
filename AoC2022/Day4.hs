module Main where

parseLine = (integer `sepBy1` char '-') `sepBy1` char ','

solve1 [x, y] = f x y || f y x where
                f [a, b] [c, d] = (a >= c) && (b <= d)

solve2 [x, y] = overlap x y || overlap y x where
                overlap [a, b] [c, d] = not (b < c || a > d)

main = readFile "Day4.txt" >>= (parseStr (parseLines parseLine) >>>
                        (fmap . fmap) (fromEnum . solve2) >>> fmap sum >>> print)
