{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Control.Applicative
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M
import qualified Data.MultiSet as S
import qualified Control.Monad.Random as R

type FreqMap = M.Map [B.ByteString] (S.MultiSet B.ByteString)

write_length :: Int
write_length = 2000

buildFreqMap :: B.ByteString -> Int -> FreqMap
buildFreqMap str chainLength = foldl addChain M.empty snippets
  where wordList = B.words str
        snippets = map (take $ chainLength + 1) $ takeWhile ((> chainLength) . length) $ iterate tail wordList
        addChain m snippet = let (chain, [next]) = splitAt chainLength snippet in
                              M.insertWith S.union chain (S.singleton next) m


nextWord :: R.RandomGen g => [FreqMap] -> [B.ByteString] -> R.Rand g B.ByteString
nextWord maps strs = let chains = iterate tail strs
                         firstFound = msum (zipWith M.lookup chains maps) in
                     getRandomElem (fromJust firstFound) -- the last chain will be [], which should always have a result

randomWrite :: R.RandomGen g => B.ByteString -> Int ->  R.Rand g B.ByteString
randomWrite input maxChainLength = let freqMaps = map (buildFreqMap input) [maxChainLength, maxChainLength-1..0]
                                       seed = replicate maxChainLength ""
                                       addWord :: R.RandomGen g => [B.ByteString] -> R.Rand g [B.ByteString]
                                       addWord lst = (: lst) <$> nextWord freqMaps (reverse $ take maxChainLength lst) in
                                   B.unwords . reverse <$> callN write_length (>>= addWord) (return seed)


main :: IO ()
main = do
        input <- B.getContents
        output <- R.evalRandIO (randomWrite input 0)
        putStrLn $ B.unpack output



getRandomElem :: R.RandomGen g => S.MultiSet a -> R.Rand g a
getRandomElem = R.fromList . map (\(x,i) -> (x, fromIntegral i)) . S.toOccurList

callN :: Int -> (a->a) -> a -> a
callN 0 _ x = x
callN n f x = callN (n-1) f (f x)
