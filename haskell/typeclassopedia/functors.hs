-- Functor instance for Either e
instance Functor (Either e) where
  fmap f (Right x) = Right (f x)
  fmap _ (Left x) = Left x

-- Functor instance for (e -> )
instance Functor ((->) e) where
  fmap f g = f . g

-- Functor instance for (e,)
instance Functor ((,) e) where
  fmap f (e,x) = (e, f x)

-- Functor instance for Pair
data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

-- Functor instance for ITree
data ITree a = Leaf (Int -> a)
             | Node [ITree a]
instance Functor ITree where
  fmap f (Leaf g) = Leaf (fmap f g)
  fmap f (Node l) = Node (map (fmap f) l)

-- An example of a type of kind * -> * which cannot be made an instance of Functor
-- > a Binary Search Tree: applying arbitrary operations to the elements may make it invalid

-- "The composition of two Functors is also a Functor." is [TODO]
-- instance (Functor f, Functor g) => Functor (f g) where
--   fmap = undefined
