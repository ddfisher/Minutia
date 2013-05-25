import Control.Monad(foldM)


powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = concatMap (\set -> [x:set, set]) (powerset xs)


powerset' :: [a] -> [[a]]
powerset' [] = [[]]
powerset' (x:xs) = powerset xs >>= (\set -> [x:set, set])


powerset'' :: [a] -> [[a]]
powerset'' = foldM (\set elem -> [elem:set, set]) []
