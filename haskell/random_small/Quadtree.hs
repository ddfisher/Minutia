{-# LANGUAGE TemplateHaskell #-}
module Quadtree where

import Prelude hiding (lookup)
import Control.Lens
import Data.NumInstances

type Point = (Double, Double)


data Box = Box { _lowerLeft :: Point
               , _upperRight :: Point
               }
  deriving Show
makeClassy ''Box

data Quadtree = Leaf { _boxQ :: Box
                     , _points :: [Point]
                     }
              | Node { _boxQ :: Box
                     , _subtrees :: [Quadtree]
                     }
  deriving Show
makeLenses ''Quadtree

instance HasBox Quadtree where
  box = boxQ

-- Box functions
inBox :: Point -> Box -> Bool
inBox (x,y) (Box (lx, ly) (ux, uy))
  =  lx <= x && x < ux
  && ly <= y && y < uy

intersectsBox :: Box -> Box -> Bool
intersectsBox (Box (lx1, ly1) (ux1, uy1))
              (Box (lx2, ly2) (ux2, uy2))
  =  lx1 < ux2 && lx2 < ux1
  && ly1 < uy2 && ly2 < uy1

splitBox :: Box -> [Box]
splitBox (Box (lx, ly) (ux, uy)) = let (mx, my) = ((lx, ly) + (ux, uy)) / 2 in
                       [ Box (lx, ly) (mx, my)
                       , Box (lx, my) (mx, uy)
                       , Box (mx, ly) (ux, my)
                       , Box (mx, my) (ux, uy)
                       ]


-- Quadtree functions

maxLeafSize :: Int
maxLeafSize = 2

empty :: Box -> Quadtree
empty b = Leaf b []


splitTree :: Quadtree -> Quadtree
splitTree (Leaf b ps) = let newNode = Node b (splitBox b & map empty) in
                        foldr insert newNode ps
splitTree _           = error "Nodes should only be split once!"

insert :: Point -> Quadtree -> Quadtree
insert p l@Leaf{} = let newLeaf = l & points %~ (p:) in
                    -- TODO: why doesn't this work?
                    -- if (newLeaf^..points.to length) > maxLeafSize then
                    if (length $ newLeaf^.points) > maxLeafSize then
                      splitTree newLeaf
                    else
                      newLeaf
insert p n@Node{} = n & subtrees.mapped %~ addPoint
  where addPoint t = if p `inBox` (t^.box) then insert p t else t


lookup :: Box -> Quadtree -> [Point]
lookup b t
  | b `intersectsBox` (t^.box)
    = case t of
     Node{} -> t^.subtrees >>= lookup b
     Leaf{} -> t^.points & filter (`inBox` b)
  | otherwise = []

fromList :: Box -> [Point] -> Quadtree
fromList b = foldr insert (empty b)
