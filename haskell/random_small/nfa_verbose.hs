module Main where

import qualified Data.IntMap as Map
import Data.List
import Data.List.Utils
import Control.Applicative
import System.IO
import System.Environment

import qualified Text.Parsec as P
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Text.Parsec.Char as P
import qualified Text.Parsec.Combinator as P

type State = Int
type Input = Char

data NFA = NFA { startStates     :: [State]
               , acceptingStates :: [State]
               , transitionMap   :: Map.IntMap [(Input, State)]
               } deriving Show

lookupAll :: Eq a => a -> [(a,b)] -> [b]
lookupAll key lst = map snd $ filter ((==key).fst) lst

transition :: (Map.IntMap [(Input, State)]) -> [State] -> Input -> [State]
transition transitionMap currentStates input = do
  state <- currentStates
  nextState <- lookupAll input (transitionMap Map.! state)
  return nextState

accepts :: NFA -> [Input] -> Bool
nfa `accepts` inputs = (acceptingStates nfa) `intersect` endStates /= []
            where endStates = foldl (transition (transitionMap nfa)) (startStates nfa) inputs


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

transitionFn :: P.Parser (Map.IntMap [(Input, State)])
transitionFn = Map.fromList <$> P.many transitionLine

nfa :: P.Parser NFA
nfa = NFA <$> stateList <*> stateList <*> transitionFn
