import Control.Applicative (liftA, (<*))
import Data.Maybe

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Token
import Text.Parsec.Language

data Expr = Number Double | Operator (Double -> Double -> Double)

instance Show Expr where
    show (Number n) = show n
    show (Operator _) = "Op"


operators :: [([Char], Double -> Double -> Double)]
operators = [("+", (+))
            ,("-", (-))
            ,("*", (*))
            ,("/", (/))]

toOperator :: String -> Maybe Expr
toOperator = (fmap Operator) . (`lookup` operators)

toOperatorCertain :: String -> Expr
toOperatorCertain = fromJust . toOperator

computeParse :: String -> Either String [Expr]
computeParse str = case parse parseExprs "(unknown)" str
                    of Left err -> Left (show err)
                       Right v -> Right v

parseExprs :: GenParser Char st [Expr]
parseExprs = many ((parseOperator <|> parseNumber) <* spaces)

parseNumber :: GenParser Char st Expr
parseNumber = fmap wrapNumber $ naturalOrFloat (makeTokenParser emptyDef)
    where wrapNumber (Right f) = Number f
          wrapNumber (Left n)  = Number (fromIntegral n)

parseOperator :: GenParser Char st Expr
parseOperator = choice $ map (liftA toOperatorCertain . string . fst) operators

calcRPN :: [Expr] -> Either String Double
calcRPN list = calcRPNStack [] list
  where calcRPNStack [n] [] = Right n
        calcRPNStack stack (Number n:restList) = calcRPNStack (n:stack) restList
        calcRPNStack (n1:n2:restStack) (Operator op:restList) = calcRPNStack (op n2 n1:restStack) restList
        calcRPNStack [] [] = Left "Nothing to calculate"
        calcRPNStack _ [] = Left "More than one number remaining on stack"
        calcRPNStack _ _ = Left "Insufficient arguments for operator"

doRPN :: String -> Either String Double
doRPN str = computeParse str >>= calcRPN


-----------------------------------

solveRPN :: String -> Double
solveRPN = head . foldl updateStack [] . words
    where updateStack (x:y:xs) "+" = (x+y):xs
          updateStack (x:y:xs) "-" = (x-y):xs
          updateStack (x:y:xs) "*" = (x*y):xs
          updateStack (x:y:xs) "/" = (x/y):xs
          updateStack stack numstr = read numstr:stack
