module Main where

mparse = count 4 letter

tripR :: String -> Char -> String -> Either Int (String, Char, String)
tripR x y z = Right (x, y, z)

parseEq = (Left <$> integer) <|>
                (tripR <$> mparse <*>
            (space *> oneOf "-*/+" <* space) <*> mparse)

parse = (,) <$> (mparse <* string ": ") <*> parseEq

getOp '*' = (*)
getOp '/' = div
getOp '-' = flip subtract
getOp '+' = (+)

type MonMap = Map String (Either Int (String, Char, String))

solve :: MonMap -> Int
solve mymap = solve' "root"
        where
            look :: String -> Either Int (String, Char, String)
            look mon = fromJust (Map.lookup mon mymap)
            solve' mon = case look mon of 
                            (Left i) -> i
                            (Right (a,op,b)) -> getOp op (solve' a) (solve' b)

eqCheck = ["hzgl","rvrh"]

solve2 :: MonMap -> String -> Either Int String
solve2 mymap = solve'
        where
            look :: String -> Either Int (String, Char, String)
            look mon = fromJust (Map.lookup mon mymap)
            solve' mon = if mon == "humn" then Right mon else 
                            case look mon of 
                                (Left i) -> Left i
                                (Right (a,op,b)) -> case  (solve' a,solve' b) of 
                                                        (Left i, Left i2) -> Left (getOp op i i2)
                                                        (Left i, Right i2) -> Right (printf "(%d %c %s)" i op i2)
                                                        (Right i, Left i2) -> Right (printf "(%s %c %d)" i op i2)
                                                        (Right i, Right i2) -> Right (printf "%s %c %s" i op i2)

main = readFile "Day21.txt" >>= (parseStr (parseLines parse) >>> fmap Map.fromList >>>
                                        fmap ((<$> eqCheck) . solve2) >>>  print)
