------------  Identity  ------------
data Identity a = Identity a
  deriving Show

instance Monad Identity where
  return = Identity
  (Identity x) >>= f = f x


------------  Maybe     ------------

instance Monad Maybe where
  return = Just
  Just x  >>= f = f x
  Nothing >>= _ = Nothing


------------  List      ------------
instance Monad [] where
  return x = [x]
  (>>=) = concatMap


---------  String Writer  ----------
data StringWriter a = StringWriter String a
  deriving Show
execStringWriter (StringWriter s x) = (s,x)
tell s = StringWriter s ()

instance Monad StringWriter where
  return = StringWriter ""
  StringWriter s x >>= f = StringWriter (s ++ newString) result
      where (newString, result) = execStringWriter (f x)


------------  Writer    ------------
------------  State     ------------
