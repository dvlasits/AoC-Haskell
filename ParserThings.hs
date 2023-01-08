module ParserThings (module ParserThings) where



import Data.Void (Void)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Control.Monad (void)
import qualified Text.Parsec.Expr as E


nonDigits :: Parser String
nonDigits = many1 (letter <|> char ' ')

symbol :: Char -> Parser ()
symbol c = void $ lexeme $ char c

on3 f k x y z = f (k x) (k y) (k z)

integer :: Parser Int
integer = read <$> many1 digit

negative :: Parser Int
negative = negate <$> (char '-' *> integer)

posNegNum :: Parser Int
posNegNum = integer <|> negative


parseLines :: Parser a -> Parser [a]
parseLines p = many1 $ p <* optional (char '\n')


parseStr :: Parser a -> String -> Either ParseError a
parseStr p = parse p "" 

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

nonNumStuff = many1 (oneOf "',.pyfgcrlntshdiueoa;qjkxbm<>PYF=GCRLNTHDIUSEOA MBXBKJQ~:")

ints :: Parser [Int]
ints = optional nonNumStuff *> many1 (integer <* optional nonNumStuff)

{-
-- Showing how to do expressions
data PlusTimesExpr = PteVar String
                   | PteNum Integer
                   | PteParens PlusTimesExpr
                   | Plus PlusTimesExpr PlusTimesExpr
                   | Times PlusTimesExpr PlusTimesExpr
                         deriving (Eq,Show)

plusTimesExpr :: Parser PlusTimesExpr
plusTimesExpr = E.buildExpressionParser pteTable pteTerm

--pteTable :: [[E.Operator PlusTimesExpr]]
pteTable = [[E.Infix (Times <$ symbol '*') E.AssocLeft]
           ,[E.Infix (Plus <$ symbol '+') E.AssocLeft]
           ]

pteTerm :: Parser PlusTimesExpr
pteTerm = pteVar <|> pteNum <|> pteParens

pteNum :: Parser PlusTimesExpr
pteNum = PteNum <$> integer
pteVar :: Parser PlusTimesExpr
pteVar = PteVar <$> identifier
pteParens :: Parser PlusTimesExpr
pteParens = PteParens <$> between (symbol '(') (symbol ')') plusTimesExpr
identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    firstChar = letter <|> char '_'
    nonFirstChar = digit <|> firstChar
-}
--End of showing how to do expressions 
