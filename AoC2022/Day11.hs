type Parser = Parsec Void Text

data Monkey = Monkey {
    _num :: Int,
    _item :: [Int],
    _operation :: Int -> Int,
    _divisBy :: Int,
    _toThrow :: Bool -> Int
}

instance Show Monkey where
    show Monkey {_num = id, _item = item} = show (id, item)

makeLenses ''Monkey 


skip :: Parser ()
skip = void $ many (noneOf ['\n']) *> newline

skipLetters :: Parser ()
skipLetters = void $ many letterChar


funcFind :: Parser (Int -> Int)
funcFind = (id <$ string "old") <|> (const <$> L.decimal)

parseEQ :: Parser (Int -> Int)
parseEQ = do
        func1 <- funcFind
        space
        op <- ((+) <$ single '+') <|> 
              (flip subtract <$ single '-') <|>
              ((*) <$ single '*')
        space
        func2 <- funcFind
        return $ \x -> op (func1 x) (func2 x)

lastInt :: Parser Int
lastInt = do
    line <- many (noneOf ['\n'])
    return . read $ last $ words line

parseMonkey :: Parser Monkey
parseMonkey = do
        num <- string "Monkey " *> L.decimal <* skip
        let lexeme = L.lexeme (void . optional $ string ", ")
        items <- string "  Starting items: " *> many (lexeme L.decimal) <* newline
        op <- string "  Operation: new = " *> parseEQ <* newline
        divis <- lastInt <* newline
        trueThrow <- lastInt <* newline
        falseThrow <- lastInt
        let toThrow = bool falseThrow trueThrow
        return $ Monkey {_num = num, _item = items, _operation = op, _divisBy = divis, _toThrow = toThrow}


doItem :: Int -> Monkey -> Int -> State (Map Int Monkey) ()
doItem modBy (Monkey {..}) oneItem = ix newMonkey . item %= (++ [afterWorry])
            where
                afterWorry = _operation oneItem `mod` modBy-- `div` 3
                newMonkey =  _toThrow ((afterWorry `rem` _divisBy) == 0)


doStep :: Int -> Int -> State (Map Int Monkey) (Int, Int)
doStep modBy num = do
        monkey <- gets (^?! ix num)
        traverse_ (doItem modBy monkey) (monkey ^. item)
        ix num . item .= []
        return (num, length $ monkey ^. item)



solve monkeyList = product . take 2 . reverse . sort . M.elems $ M.fromListWith (+) (concat afterRounds)
        where   
            numMonkeys = length monkeyList
            monkeyMap = M.fromList . zipFrom 0 $ monkeyList
            modBy = productOf (traversed . divisBy) monkeyMap
            afterRounds = evalState (replicateM 10000 (traverse (doStep modBy) [0..(numMonkeys - 1)])) monkeyMap


main = do
    input <- readFile "day11.txt" 
    let parsed = T.splitOn "\n\n" >>> traverse (parse parseMonkey "") $ input
        solved = fmap solve parsed
    print solved
