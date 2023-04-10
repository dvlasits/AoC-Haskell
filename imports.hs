{-# LANGUAGE TupleSections, ViewPatterns, PatternSynonyms #-}

import Data.List.Split (divvy, splitOn, endBy, splitWhen, splitOneOf, chunksOf)

--No monomorphism questionable
--import System.IO.Strict

import Text.Parsec (anyChar, choice, try, eof, many, ParseError, skipMany1, sepBy, endBy, endBy1,
                            sepEndBy, sepEndBy1, eof, notFollowedBy, manyTill, between, space, sepBy1)
import Text.Parsec.Error
import Text.Parsec.Combinator (many1, choice, chainl1, chainl1, chainr, chainr1 , between
                                     ,count, option, optionMaybe, optional)
-- chainl1 takes in a Parser a and a Parser (a -> a -> a) and will return all funcs applied to a's
-- chainl1 guarantees at least 1 and chainl requires 1 extra argument which is leftover
-- chainl and chainr do left or right association
-- <$ used if have no argument constructor
import Text.Parsec.Char (oneOf, noneOf, satisfy, upper, lower, alphaNum,
                             letter, digit, hexDigit, octDigit, space, char, string, newline)
import Text.Parsec.Token (parens)
import Control.Arrow ((>>>), (&&&), (***))
-- (***) (+3) (*4) (1,2) == (4,8) and join f a = f a a!!!
import ParserThings
import Text.Parsec.String

import Data.Char (isLetter, isDigit, ord, isUpper)
import Control.Monad (void, ap, mzero, (<=<), (>=>), forever, join)
-- ap does m (a -> b) -> m a -> m b
-- try to understand guard, when and unless
import Data.Monoid
import Data.List 
import Data.List (splitAt, dropWhileEnd, span, stripPrefix, inits, tails, isPrefixOf, isSuffixOf, isInfixOf, 
                                isSubsequenceOf, elem, notElem, lookup, find, partition, elemIndex, elemIndices, findIndex, 
                                nub, delete, (\\), union, intersect, sortOn, insert, nubBy, deleteBy, maximumBy)
-- find will find first item that satisfies predicate, partition splits list in two based on predicate
-- Delete removes first occurence satisfying predicate, nub removes duplicates, \\ list difference, 
-- insert will put into first place where it is <= right element
-- Remember you can put By's in front, generic nice to remember if ints screwing you

import Control.Monad.State
import Control.Applicative (liftA2, liftA3, (<$>), (<*>), (<$), (<*), (*>), (<|>), (<**>))

-- Wanan go mad there is also <**> which is flipped
import Data.Either (isLeft, isRight, either, fromRight) -- Though ||| nicer
import Data.Bool (bool)
import Data.Maybe (fromJust, isJust, isNothing, fromMaybe, catMaybes, mapMaybe)--, mayMaybe)
--import Data.Graph as G
import Data.Set (Set, lookupMin, lookupMax)
import qualified Data.Set as Set 
import Data.Map (Map, fromListWith, (!?)) -- Look at alter and adjust, intersectionWith, unionWith)
import qualified Data.Map.Strict as Map
import Data.Sequence (Seq, (><), insertAt, deleteAt, index) --mapWithIndex, foldrWithIndex
import qualified Data.Sequence as Seq

import Data.Tuple
import Data.Function (on, (&))
import Data.Ord (comparing)
import Debug.Trace (traceShowId, traceShow)
import Text.Parsec.Token (lexeme, makeTokenParser, whiteSpace)
import Text.Parsec.Language (emptyDef)
--func x | trace (show x) $ False = undefined
--LOOK AT THESE
import Data.Bifunctor (bimap, first, second) --Useful for tuples and either
import Data.Bifoldable (biconcatMap,bifold, bifoldMap, bifoldr, bifoldl', biconcat, biList, biany, biall, biproduct)
import Data.Bitraversable (bitraverse, bisequenceA)
-- biconcat t [a] [a] -> [a] and biconcatMap will map first


import Data.Foldable (fold, toList)
import Criterion.Main
import Control.Lens hiding (element, noneOf)
import Control.Lens.TH
import Data.Graph.Inductive (Gr, Node)
import qualified Data.Graph.Inductive as G
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Text.Printf
import Algorithm.Search


instance Num a => Num (a,a) where
    (x,y) + (x', y') = (x+x', y+y')
    (x,y) * (x', y') = (x*x', y*y')
    abs (x, y) = (abs x, abs y)
    fromInteger x = (fromIntegral x,fromIntegral x)
    signum (x ,y) = (signum x, signum y)
    negate (x, y) = (-x, -y)
