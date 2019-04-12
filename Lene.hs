module Lene where

import Prelude hiding (foldr, sum, map, sort)

doi :: Integer -> Integer
doi _ = 2

plus :: Integer -> Integer -> Integer
plus x y = x + y

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = (f x) : (map f xs)

data Tree a = Leaf a | Node (Tree a) (Tree a)

foldTree :: (a -> b) -> (b -> b -> b) -> Tree a -> b
foldTree leaf node (Leaf x) = leaf x
foldTree leaf node (Node left right) = node l r
    where
        l = foldTree leaf node left
        r = foldTree leaf node right

fold :: (a -> b -> b) -> b -> [a] -> b
fold f k [] = k
fold f k (x: xs) = f x (fold f k xs)

lungime :: [a] -> Int
lungime [] = 0
lungime (_:xs) = 1 + lungime xs

lungime2 :: [a] -> Integer
lungime2 = fold il 0 where
    il :: a -> Integer -> Integer
    il _ x = x + 1

interclasare :: [Integer]->[Integer] ->[Integer]
interclasare xs [] = xs
interclasare [] ys = ys
interclasare (x:xs) (y:ys)
    | x > y = x : interclasare xs (y:ys)
    | otherwise = y : interclasare (x:xs) ys

sort :: [Integer] -> [Integer]
sort [x] = [x]
sort [] = []
sort xs = interclasare l r
    where
        lg = lungime xs
        l = sort (take (lg `div` 2) xs)
        r = sort (drop (lg `div` 2) xs)
