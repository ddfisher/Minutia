import Control.Arrow

instance Arrow (->) where
  arr = id
  (>>>) = (.)
  first f = \(b, d) -> (f b, d)
