import Data.Ord
import Data.List
longestIncSubSeq :: Ord a => [a] -> [a]
longestIncSubSeq = buildSubSeq []
  where buildSubSeq subseq [] = subseq
        buildSubSeq subseq (first:rest)
            | not (null subseq) && (last subseq) >= first = buildSubSeq subseq rest
            | otherwise = maximumBy (comparing length) [ buildSubSeq subseq rest
                                                       , buildSubSeq (subseq ++ [first]) rest]


longestIncSubSeq' :: Ord a => [a] -> [a]
longestIncSubSeq' = buildSubSeq []
  where buildSubSeq subseq [] = subseq
        buildSubSeq subseq (first:rest)
            | null subseq || (last subseq) < first = maximumBy (comparing length)
                                                               [pickedSubSeq, unpickedSubSeq]
            | otherwise                            = unpickedSubSeq
          where pickedSubSeq   = buildSubSeq (subseq ++ [first]) rest
                unpickedSubSeq = buildSubSeq subseq rest


longestIncSubSeq'' :: Ord a => [a] -> [a]
longestIncSubSeq'' = buildSubSeq []
  where buildSubSeq subseq [] = subseq
        buildSubSeq subseq (first:rest)
            | null subseq || (last subseq) < first
              = maximumBy (comparing length)
                          [buildSubSeq (subseq ++ [first]) rest, buildSubSeq subseq rest]
            | otherwise = unpickedSubSeq
