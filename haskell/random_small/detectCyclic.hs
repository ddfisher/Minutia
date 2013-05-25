

detectCyclic :: Eq a => [a] -> Bool
detectCyclic l = h l (drop 1 l)
  where h _    [] = False
        h slow fast = if head slow == head fast then
                        True
                      else
                        h (drop 1 slow) (drop 2 fast)
