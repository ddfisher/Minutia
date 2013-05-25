{-# LANGUAGE TemplateHaskell #-}
module FirstForray where

import Language.Haskell.TH

tupleReplicate :: Int -> Q Exp
tupleReplicate n = do
  id <- newName "x"
  return $ LamE [VarP id] (TupE $ replicate n (VarE id))

tupleReplicate' :: Int -> Q Exp
tupleReplicate' n = [| \x -> $(tupE $ replicate n $ [| x |]) |]


sel :: Int -> Int -> Q Exp
sel index length = do
  id <- newName "x"
  let pat = replicate index WildP ++ [VarP id] ++ replicate (length - index - 1) WildP
  return $ LamE [TupP pat] (VarE id) 


makeReplicator :: String -> Int -> DecsQ
makeReplicator s n = do
  id <- newName "x"
  let cl = clause [varP id] (normalB $ tupE $ replicate n $ varE id) []
  f <- funD (mkName s) [cl]
  return [f]

-- Splicing names isn't currently supported, unfortunately
-- makeReplicator' :: Name -> Int -> DecsQ
