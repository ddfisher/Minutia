module Main where

import qualified Data.IntMap as Map
import Control.Monad
import Control.Applicative
import System.IO
import System.Environment

import qualified Text.Parsec as P
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Combinator as P

data NFA state input = NFA { start      :: [state]
                           , accepting  :: state -> Bool
                           , transition :: state -> input -> [state]
                           }



accepts :: NFA s i -> [i] -> Bool
(NFA st a tr) `accepts` input = any a $ (st >>= \s -> foldM tr s input)

type State = Int
type Input = Char


main = do
  [filename, input] <- getArgs
  contents <- readFile filename
  case P.parse nfa filename contents of
    Left error -> print error
    Right aut  -> putStrLn (if aut `accepts` input then "Accepts" else "Rejects")

-- parsing --
spaces :: P.Parser [Char]
spaces = P.many (P.oneOf " \t" )

input :: P.Parser Input
input = P.alphaNum

state :: P.Parser State
state = read <$> (P.many1 P.digit P.<?> "state")

stateList :: P.Parser [State]
stateList = state `P.sepBy` spaces <* P.newline P.<?> "list of states"

edge :: P.Parser (Input, State)
edge = P.between (P.string "(") (P.string ")") $
                      (,) <$> (input <* P.string "," <* spaces)
                          <*> state

transitionLine :: P.Parser (State, [(Input, State)])
transitionLine = (,) <$> (state <* spaces <* P.string ":" <* spaces)
                     <*> P.many1 (edge <* spaces) <* P.newline

transitionFn :: P.Parser (State -> Input -> [State])
transitionFn = (mapToFn . Map.fromList) <$> P.many transitionLine
  where mapToFn m state input = lookupAll input (m Map.! state)

lookupAll :: Eq a => a -> [(a,b)] -> [b]
lookupAll key lst = map snd $ filter ((==key).fst) lst

nfa :: P.Parser (NFA State Input)
nfa = NFA <$> stateList <*> ((\sts -> (`elem` sts)) <$> stateList) <*> transitionFn
