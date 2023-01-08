data Rec = L [Rec] | I Int deriving (Show)

comma = optional (char ',')

parsell = L <$> (char '[' *> many ((I <$> (integer <* comma)) <|> parsell) <* char ']' <* comma)

pair = (,) <$> parsell <*> (whitespace *> parsell <* optional whitespace)

instance Eq Rec where
    x == y = (x `compare` y) == EQ

instance Ord Rec where
    (I x) `compare` (I y) = x `compare` y
    (L x) `compare` (I y) = L x `compare` L [I y]
    (I x) `compare` (L y) = L [I x] `compare` L y
    (L []) `compare` (L []) = EQ
    (L []) `compare` (L (_:_)) = LT
    (L (_:_)) `compare` (L []) = GT
    (L (x:xs)) `compare` (L (y:ys)) = case x `compare` y of
                                            LT -> LT
                                            GT -> GT
                                            EQ -> L xs `compare` L ys

singleton x = [x]

solve1 :: [(Rec, Rec)] -> Int
solve1 = sum . map fst . filter snd . zip [1..] . map (uncurry (<))

dividers = [L [L [I 2]], L [L [I 6]]]

solve2 :: [(Rec, Rec)] -> Int
solve2 = product . map (+1) . catMaybes  . liftA2 elemIndex dividers .
                singleton . sort . (dividers++) . uncurry (++) . unzip


main = readFile "Day13.txt" >>= (parseStr (parseLines pair) >>> fmap solve2 >>> print)
