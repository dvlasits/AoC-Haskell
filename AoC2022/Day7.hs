module Main where

data CDOp = Back | Root | Folder String deriving Show

data Command = CD CDOp | LS Int deriving Show

toEnd = (many1 . noneOf) "\n"

parseCD = CD <$> (string "cd " *> (
            (Root <$ char '/') <|> 
            (Back <$ string "..") <|> 
            (Folder <$> many1 letter)) <* newline)

parseLS = string "ls\n" *> (LS . sum <$> many (integer <* toEnd <* optional newline))

parse = many1 $ string "$ " *> (parseCD <|> parseLS)

insertByLabel :: (Node, Gr (Sum Int) String) -> String -> (Node, Gr (Sum Int) String)
insertByLabel (node, g) label = case lookup label (map swap (G.lsuc g node)) of
                                        Just i ->  (i, g)
                                        Nothing -> let [i] = G.newNodes 1 g in (i, 
                                                        ([(label, node)], i, Sum 0, []) G.& g)

oneStep :: (Node, Gr (Sum Int) String) -> Command -> (Node, Gr (Sum Int) String)
oneStep (node, g) (CD Back)  = (head $ G.pre g node, g)
oneStep (node, g) (CD Root)  = (1, g)
oneStep (node, g) (CD (Folder str))  = insertByLabel (node, g) str
oneStep (node, g) (LS filesizes)  = case G.match node g of 
                                        (Just x,g2) -> (node, (x & _3 .~ Sum filesizes) G.& g2)

createGraph = snd . foldl' oneStep (1,G.insNode (1, Sum 0) G.empty)

getFileSizes :: Gr (Sum Int) String -> [Sum Int]
getFileSizes = snd . G.gfold G.suc' dagg ((<>) . fromJust, (Sum 0, [])) [1]
        where
            dagg cont (csize, directories) = let val = csize + G.lab' cont in (val, val:directories)

solve1 = sum . filter (<= 100000)
solve2 = minimum . (filter . (<) =<< (subtract 40000000 . head))

main = readFile "day7.txt" >>= (unlines . filter (not . ("dir" `isPrefixOf`)) . lines >>>
                                parseStr parse >>> fmap createGraph >>> 
                                fmap (solve2 . getFileSizes) >>> print)
