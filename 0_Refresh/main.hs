import Data.Char as Char
--unwords using direct recursion
unwords' :: [String] -> String
unwords' [] = ""
unwords' (x:[]) = x
unwords' (x:xs) = x ++ " " ++ unwords' xs

--unwords using foldr
unwords'' :: [String] -> String
unwords'' = foldr addSpace "" 

addSpace :: String -> String -> String
addSpace x "" = x
addSpace x y = x ++ " " ++ y

--words using recursion and helper functions like takeWhile ???????
--words' :: String -> [String]
words' "" = []
words' s = a : words' (drop (length(a)) s)
    where a = takeTillSpace s
takeTillSpace = takeWhile (not . Char.isSpace)

--words using foldr
words'' s = foldr (\c (x:xs) ->
                if c == ' '
                then "":x:xs
                else (c:x):xs
            ) [""] s

--2
--foldl
--https://wiki.haskell.org/Fold

--3
numberGen :: [Int] -> Int
numberGen x = foldl(\x y -> 10*x + y) 0 x

stringParse :: String -> Int
stringParse x = numberGen $ map digitToInt x

--4
data Tree a = Bin (Tree a) (Tree a) | Tip a

treee = Bin (Bin (Tip 1) (Tip 2)) (Tip 3) 

treeToList :: (Ord a) => Tree a -> [a]   
treeToList (Tip a) = [a]      
treeToList (Bin left right) = treeToList left ++ treeToList right 

--5
