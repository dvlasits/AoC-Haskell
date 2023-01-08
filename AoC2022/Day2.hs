module Main where

parseLine = (,) <$> (letter <* char ' ') <*> letter

win = [('A', 'Y'), ('B', 'Z'), ('C', 'X')]
draw = [('A', 'X'), ('B', 'Y'), ('C', 'Z')]
lose = [('A', 'Z'), ('B', 'X'), ('C', 'Y')]
rule = [('X', lose), ('Y', draw), ('Z', win)]

points = [('X', 1), ('Y', 2), ('Z', 3)]

getI val l = fromJust (val `lookup` l)

point c@(c1, c2) =  (c2 `getI` points) +
                    bool (bool 0 3 (c `elem` draw)) 6 (c `elem` win)

setCorrect (c1, c2) = (c1, c1 `getI` (c2 `getI` rule))

main = readFile "Day2.txt" >>= 
    (parseStr (parseLines parseLine) >>> fmap (map setCorrect) >>>
        fmap (sum . map point) >>> print)
