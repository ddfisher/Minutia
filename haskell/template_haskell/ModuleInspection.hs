{-# LANGUAGE TemplateHaskell #-}
module ModuleInspection where

import Language.Haskell.TH
import Foreign.C
import Control.Monad

data SimpleType = UnknownT | IntT deriving (Show, Eq)

makePythonExport :: Name -> DecsQ
makePythonExport nm = do
  VarI _ typ _ _ <- reify nm
  let types = extractTypes typ
  vars <- mapM (const $ newName "arg") types

  pythonName <- mkName $ show nm ++ "_python"
  let cl = clause (map varP vars) (normalB
  f <- funD pythonName [cl]



-- a couple utility definitions

-- args (a -> b -> c -> d) = [a,b,c]
args (AppT (AppT ArrowT x) y) = x : args y
args _ = []

-- result (a -> b -> c -> d) = d
result (AppT (AppT ArrowT _) y) = result y
result y = y

-- con (IO a) = IO
-- con (a,b,c,d) = TupleT 4
con (AppT x _) = con x
con x = x

-- conArgs (a,b,c,d) = [a,b,c,d]
-- conArgs (Either a b) = [a,b]
conArgs ty = go ty [] where
    go (AppT x y) acc = go x (y:acc)
    go _ acc = acc


getTypes :: Name -> Q ([SimpleType], SimpleType)
getTypes nm = do
  VarI _ typ _ _ <- reify nm
  return $ extractTypes typ


extractTypes :: Type -> ([SimpleType], SimpleType)
extractTypes = extractTypes' []
  where
    extractTypes' ts (AppT (AppT ArrowT typ) rest) = extractTypes' (ts ++ [convertType typ]) rest
    extractTypes' ts typ@(ConT _)                  = (ts, convertType typ)

convertType :: Type -> SimpleType
convertType (ConT x) | x == ''Int = IntT
convertType _                     = UnknownT
