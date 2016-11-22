sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum'' :: Num a => [a] -> a
sum'' = foldr (+) 0

length' :: [a] -> Int
length' [] = 0
length' (x : xs) = 1 + length' xs

length'' :: [a] -> Int
length'' = foldr (\_ n -> 1 + n) 0

min' :: Ord a => [a] -> a
min' [] = error "Empty list"
min' (x:[]) = x
min' (x:xs) = minHelp xs x

minHelp [] m = m 
minHelp (x:xs) m = if x < m then minHelp xs x else minHelp xs m

min'' :: Ord a => [a] -> a
min'' [] = error "Empty list"
min'' (x:xs) = foldr (\x y -> if x < y then x else y) x xs 

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f x = foldr (\c x' -> (f c):x') [] x