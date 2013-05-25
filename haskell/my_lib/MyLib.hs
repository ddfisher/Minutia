module MyLib where

infixl 0 &
(&) :: a -> (a -> b) -> b
(&) = flip ($)
