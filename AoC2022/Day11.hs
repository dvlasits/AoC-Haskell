type Parser = Parsec Void Text

data Monkey = Monkey {_num :: Int, _item :: [Int], _operation :: Int -> Int, _divisBy :: Int, _toThrow :: Bool -> Int}
makeLenses ''Monkey 

parseEQ :: Parser (Int -> Int)
parseEQ = do
        op <- (((+) <$ single '+') <|> ((*) <$ single '*')) <* space
        func <- (id <$ string "old") <|> (const <$> L.decimal)
        return $ op <*> func
        
parseMonkey :: Parser Monkey
parseMonkey = do
        num <- string "Monkey " *> L.decimal <* single ':' <* newline 
        items <- string "  Starting items: " *> (L.decimal `sepBy` string ", ") <* newline
        op <- string "  Operation: new = old " *> parseEQ <* newline
        rest <- T.unpack <$> takeRest
        let [divis, trueThrow, falseThrow] = fmap (read . last . words) . lines $ rest
        return $ Monkey num items op divis (bool falseThrow trueThrow)

doItem :: Int -> Monkey -> Int -> State (Map Int Monkey) ()
doItem modBy (Monkey {..}) oneItem = ix newMonkey . item %= (++ [afterWorry])
            where
                afterWorry = _operation oneItem `mod` modBy --`div` 3
                newMonkey =  _toThrow ((afterWorry `rem` _divisBy) == 0)

doStep :: Int -> Int -> State (Map Int Monkey) (Int, Int)
doStep modBy num = do
        monkey <- gets (^?! ix num)
        traverse_ (doItem modBy monkey) (monkey ^. item)
        ix num . item .= []
        return (num, length $ monkey ^. item)

solve monkeyList = product . take 2 . reverse . sort . M.elems . M.fromListWith (+) . concat $ afterRounds
        where   
            monkeyMap = M.fromList . zipFrom 0 $ monkeyList
            modBy = productOf (traversed . divisBy) monkeyMap
            afterRounds = (evalState ?? monkeyMap) . replicateM 10000 . traverse (doStep modBy) $ [0..length monkeyList  - 1]

main = do
    input <- readFile "day11.txt" 
    let parsed = T.splitOn "\n\n" >>> traverse (parse parseMonkey "") $ input
        solved = fmap solve parsed
    print solved
