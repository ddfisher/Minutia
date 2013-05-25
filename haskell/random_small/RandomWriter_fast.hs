{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import Data.Array
import Data.Random.Extras (choiceArray)
import Data.RVar
import Data.Random.Source.IO()
import Data.List(foldl')

type FreqMap = M.Map [B.ByteString] (Array Int B.ByteString)

write_length :: Int
write_length = 2000

buildFreqMap :: B.ByteString -> Int -> FreqMap
buildFreqMap str chainLength = M.map (\l -> listArray (0, length l - 1) l) $ foldl' addChain M.empty snippets
  where wordList = B.words str
        snippets = map (take $ chainLength + 1) $ takeWhile ((> chainLength) . length) $ iterate tail wordList
        addChain m snippet = let (chain, [next]) = splitAt chainLength snippet in
                              M.alter (\v -> fmap (next:) (v <|> Just [])) chain m


nextWord maps strs = let chains = iterate tail strs
                         firstFound = msum (zipWith M.lookup chains maps) in
                     choiceArray (fromJust firstFound) -- the last chain will be [], which should always have a result

randomWrite input maxChainLength = let freqMaps = map (buildFreqMap input) [maxChainLength, maxChainLength-1..0]
                                       seed = replicate maxChainLength ""
                                       addWord lst = (: lst) <$> nextWord freqMaps (reverse $ take maxChainLength lst) in
                                   B.unwords . reverse <$> callN write_length (>>= addWord) (return seed)


main :: IO ()
main = do
        input <- B.getContents
        output <- sampleRVar (randomWrite input 0)
        putStrLn $ B.unpack output


callN :: Int -> (a->a) -> a -> a
callN 0 _ x = x
callN n f x = callN (n-1) f (f x)
