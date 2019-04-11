module Lene where

import Prelude hiding (foldr)

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ k [] = k
foldr f k (x:xs) = f x (foldr f k xs)

sum :: [Integer] -> Integer
sum = foldr (+) 0
