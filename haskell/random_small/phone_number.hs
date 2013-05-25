import qualified Data.Set as Set
import Data.Maybe
import Control.Monad
import qualified System.IO.UTF8 as UTF8

wordListFilename = "english_filtered.txt"

numberToChars :: Char -> Maybe [Char]
numberToChars '2' = Just "abc"
numberToChars '3' = Just "def"
numberToChars '4' = Just "ghi"
numberToChars '5' = Just "jkl"
numberToChars '6' = Just "mno"
numberToChars '7' = Just "pqrs"
numberToChars '8' = Just "tuv"
numberToChars '9' = Just "wxyz"
numberToChars _ = Nothing

validWords :: Set.Set String -> String -> Bool
validWords wordDict start_str = any null $ partialParsing [""] start_str
    where partialParsing strs "" = strs >>= partialConsume
          partialParsing strs (x:xs) = let consumedStrs = strs >>= partialConsume
                                       in partialParsing (map (++ [x]) consumedStrs) xs
          partialConsume str | str `Set.member` wordDict = [str, ""]
                             | otherwise           = [str]

validWords' :: Set.Set String -> String -> Bool
validWords' wordDict start_str = validSoFar "" start_str
    where validSoFar "" ""      = True
          validSoFar str ""     = str `Set.member` wordDict
          validSoFar str (x:xs) | str `Set.member` wordDict = validSoFar "" xs
                                                             || validSoFar (str ++ [x]) xs
                                | otherwise                 = validSoFar (str ++ [x]) xs

combinations :: [[a]] -> [[a]]
combinations [] = [[]]
combinations (list:lists) = do
                            suffixCombination <- combinations lists
                            map (:suffixCombination) list

main = do
        wordList <- UTF8.readFile wordListFilename
        let wordSet = Set.fromList $ lines wordList
        process_numbers wordSet

process_numbers wordSet = do
        phoneNumber <- getLine
        unless (null phoneNumber) $ do
            let phoneLetters = catMaybes $ map numberToChars phoneNumber
            print $ filter (validWords wordSet) (combinations phoneLetters)
            process_numbers wordSet
