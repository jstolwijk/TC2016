import Data.Char as Char

unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:[]) = x
unwords' (x:xs) = x ++ " " ++ unwords' xs

unwords'' :: [String] -> String
unwords'' = foldr addSpace ""

addSpace :: String -> String -> String
addSpace x "" = x
addSpace x y = x ++ " " ++ y

words' :: String -> [String]
words' "" = []
words' (' ': xs) = words' xs
words' s = w : words' (drop (length(w)) s)
    where w = takeWhile (not . Char.isSpace) s

words'' s = foldr (\c (x:xs) ->
                if c == ' '
                then "":x:xs
                else (c:x):xs
            ) [""] s

foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' f init [] = init
foldl' f init (x:xs) = f x (foldl' f init xs)

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl (\x y -> 10 * x + y) 0


data Tree a = Bin (Tree a) (Tree a) | Tip a

treee = Bin (Bin (Bin(Tip 1) (Tip 1)) (Tip 2)) (Tip 3) 

information :: Tree a -> [a]
information (Tip a) = [a]
information (Bin left right) = information left ++ information right 


treeFold f init (Tip a) = f a
treeFold f init (Bin left right) = f left