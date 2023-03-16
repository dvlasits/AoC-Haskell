{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DeriveAnyClass, TemplateHaskell, RecordWildCards #-}

module Main where

import BasePrelude hiding (readFile)
import Data.List.Split (divvy, splitOn, endBy, splitWhen, splitOneOf, chunksOf)
import Control.Lens hiding (noneOf)
import Data.List.Extra (groupSortOn, minimumOn, maximumOn, zipFrom)
import Control.Monad.Extra (whenM)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Control.Monad.State
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import System.Random
import Data.Functor.Syntax
import Data.Graph.Inductive (Gr, Node)
import qualified Data.Graph.Inductive as G
import Control.Monad.Reader
import Text.Megaparsec hiding (State, many, some)
import Text.Megaparsec.Char
import Data.Void 
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text.IO hiding (getLine, putStrLn)
import Control.Lens.Unsound -- adjoin and lensProduct are cool
import Control.Exception
import Control.Monad.Except
import Control.Lens.TH
import Control.Lens.Traversal

type Parser = Parsec Void Text

data Monkey = Monkey {_num :: Int, _item :: [Int], _operation :: Int -> Int, _divisBy :: Int, _toThrow :: Bool -> Int}
makeLenses ''Monkey 

parseEQ :: Parser (Int -> Int)
parseEQ = do
        op <- (((+) <$ single '+') <|> ((*) <$ single '*')) <* space
        func <- (id <$ string "old") <|> (const <$> L.decimal)
        return $ op <*> func

lastInt :: Parser Int
lastInt = do
    line <- many (noneOf ['\n'])
    return . read $ last $ words line

parseMonkey :: Parser Monkey
parseMonkey = do
        num <- string "Monkey " *> L.decimal <* single ':' <* newline 
        items <- string "  Starting items: " *> (L.decimal `sepBy` string ", ") <* newline
        op <- string "  Operation: new = old " *> parseEQ <* newline
        divis <- lastInt <* newline
        trueThrow <- lastInt <* newline
        falseThrow <- lastInt
        return $ Monkey num items op divis (bool falseThrow trueThrow)

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

solve monkeyList = product . take 2 . reverse . sort . M.elems . M.fromListWith (+) . concat $ afterRounds
        where   
            monkeyMap = M.fromList . zipFrom 0 $ monkeyList
            modBy = productOf (traversed . divisBy) monkeyMap
            afterRounds = evalState (replicateM 10000 (traverse (doStep modBy) [0..length monkeyList  - 1])) monkeyMap

main = do
    input <- readFile "day11.txt" 
    let parsed = T.splitOn "\n\n" >>> traverse (parse parseMonkey "") $ input
        solved = fmap solve parsed
    print solved
