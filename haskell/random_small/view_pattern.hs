{-# LANGUAGE ViewPatterns #-}
import qualified Data.Sequence as S
import qualified Data.Map as M

s = S.fromList [1..10]

showReverse :: S.Seq Integer -> String
showReverse (S.viewl -> S.EmptyL) = ""
showReverse (S.viewl -> x S.:< xs) = showReverse xs ++ " " ++ show x

main = print $ showReverse s


revList [] = []
revList (x:xs) = revList xs ++ [x]

revSeq :: S.Seq a -> S.Seq a
revSeq s = case S.viewl s of
                S.EmptyL -> s
                x S.:< xs -> revSeq xs S.|> x

revSeq' :: S.Seq a -> S.Seq a
revSeq' s@(S.viewl -> S.EmptyL) = s
revSeq' (S.viewl -> x S.:< xs) = revSeq' xs S.|> x


defaultLookup :: M.Map Integer Integer -> Integer -> Integer
defaultLookup m ((`M.lookup` m) -> Nothing) = 0
defaultLookup m ((`M.lookup` m) -> Just x) = x
