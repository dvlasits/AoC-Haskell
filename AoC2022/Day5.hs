module Main where

parser = do
    crates <- parseLines  $ 
        ((Just <$> (char '[' *> letter <* char ']')) 
                    <|> try (Nothing <$ string "   ")) `sepBy1` char ' '
    many1 (noneOf "m")
    moves <- parseLines $ nonDigits *> (integer `sepBy1` nonDigits)
    return ((map catMaybes . transpose) crates, 
                map (zipWith subtract [0,1,1]) moves)

moveCrates f = foldl' move
        where
            move :: [String] -> [Int] -> [String]
            move seq [amount, from, to] = seq &~ do
                                            prev <- ix from <<%= drop amount
                                            ix to %= (f (take amount prev) ++)

[solve1, solve2] = map moveCrates [reverse, id]

main = readFile "day5.txt" >>= (parseStr parser >>>
                                fmap (uncurry solve2) >>> (fmap . fmap) head >>> print)
