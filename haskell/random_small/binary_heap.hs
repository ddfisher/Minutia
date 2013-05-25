module BinaryHeap where

data Heap a = Node { leftChild  :: Heap a
                   , rightChild :: Heap a
                   , getValue   :: a
                   }
              | Null
    deriving Show


singleton :: a -> Heap a
singleton x = Node Null Null x

insert :: a -> Heap a -> Heap a
-- insert val (Node left right nodeVal)
insert val Null = singleton val


getMin :: Ord a => Heap a -> a
getMin Null = error "Cannot get min of empty heap!"
getMin h    = getValue h

deleteMin :: Ord a => Heap a -> Heap a
deleteMin Null = error "Cannot delete min from empty heap!"
deleteMin (Node left right _)
  | getValue left <= getValue right = Node (deleteMin left) right (getMin left)
  | otherwise = Node left (deleteMin right) (getMin right)

