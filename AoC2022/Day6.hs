module Main where 

solve num = fmap (+num) . findIndex (((==) `on` length) <*> nub) . divvy num 1

[solve1, solve2] = map solve [4,14]

main = readFile "day6.txt" >>= (solve2 >>> print)
