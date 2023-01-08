
import Text.Parsec (choice, (<|>), try, eof, many, ParseError, skipMany1, sepBy, endBy, endBy1,
                            sepEndBy, sepEndBy1, eof, notFollowedBy, manyTill, between, space)
import Text.Parsec.Error
import Text.Parsec.Combinator (many1, choice, chainl1, chainl1, chainr, chainr1 , between
                                     ,count, option, optionMaybe, optional)
-- chainl1 takes in a Parser a and a Parser (a -> a -> a) and will return all funcs applied to a's
-- chainl1 guarantees at least 1 and chainl requires 1 extra argument which is leftover
-- chainl and chainr do left or right association
-- <$ used if have no argument constructor
import Text.Parsec.Char (oneOf, noneOf, satisfy, upper, lower, alphaNum,
                             letter, digit, hexDigit, octDigit, space, char, string)
import Text.Parsec.Token (parens)
import Control.Arrow ((>>>), (&&&), (***))
-- (***) (+3) (*4) (1,2) == (4,8) and join f a = f a a!!!
import ParserThings
import Text.Parsec.String

import Data.Char (isLetter, isDigit)
import Control.Monad (void, ap, mzero, (<=<), (>=>), forever, join)
-- ap does m (a -> b) -> m a -> m b
-- try to understand guard, when and unless
import Data.Monoid
import Data.List 
import Data.List (splitAt, dropWhileEnd, span, stripPrefix, inits, tails, isPrefixOf, isSuffixOf, isInfixOf, 
                                isSubsequenceOf, elem, notElem, lookup, find, partition, elemIndex, elemIndices, findIndex, 
                                findIndices, nub, delete, (\\), union, intersect, sortOn, insert, nubBy, deleteBy, maximumBy)
-- find will find first item that satisfies predicate, partition splits list in two based on predicate
-- Delete removes first occurence satisfying predicate, nub removes duplicates, \\ list difference, 
-- insert will put into first place where it is <= right element
-- Remember you can put By's in front, generic nice to remember if ints screwing you

import Control.Monad.State
import Control.Applicative (liftA2, liftA3, (<$>), (<*>), (<$), (<*), (*>), (<|>), many)
-- Wanan go mad there is also <**> which is flipped
import Data.Either (isLeft, isRight, either) -- Though ||| nicer
import Data.Bool (bool)
import Data.Maybe (fromJust, isJust, isNothing, fromMaybe, catMaybes)--, mayMaybe)
import Data.Graph as G
import qualified Data.Set as S
import Data.Tuple
import Data.Function (on, (&))
import Data.Ord (comparing)
import Debug.Trace (traceShowId)
import Text.Parsec.Token (lexeme, makeTokenParser, whiteSpace)
import Text.Parsec.Language (emptyDef)
--func x | trace (show x) $ False = undefined
--LOOK AT THESE
import Data.Bifunctor (bimap, first, second) --Useful for tuples and either
import Data.Bifoldable (biconcatMap,bifold, bifoldMap, bifoldr, bifoldl', biconcat, biList, biany, biall)
import Data.Bitraversable (bitraverse, bisequenceA)
-- biconcat t [a] [a] -> [a] and biconcatMap will map first
import qualified Data.Map as M
import Data.Foldable (fold)
