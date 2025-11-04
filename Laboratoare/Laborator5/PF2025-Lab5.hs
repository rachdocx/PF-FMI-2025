import Data.Text.Array (equal)
import Data.List (permutations, insert)
--1)
sumaPatrate :: [Int] -> Int
sumaPatrate xs = foldr (+) 0 (map (^2) (filter odd xs))

--2)
allTrue :: [Bool] -> Bool
allTrue xs = foldr (&&) True xs

--3)
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies p xs = foldr (&&) True (map (\x -> if p x then True else False) xs)

--4)
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies p xs = foldr (||) False (map (\x -> if p x then True else False) xs)
--5)
--a)
mapFoldr :: (a -> b) -> [a] -> [b] 
mapFoldr p = foldr (\x xs -> p x:xs) []
--b)
filterFoldr :: (a -> Bool) -> [a] -> [a]
filterFoldr p = foldr (\x xs -> if p x then x:xs else xs) []

--6)
listToInt :: [Integer]-> Integer
listToInt = foldr (\x y -> x + 10 * y) 0
--7)
--a)
rmChar :: Char -> String -> String
rmChar c str = foldr (\x xs -> if x == c then xs else x:xs) [] str
--b)
rmCharsRec :: String -> String -> String
rmCharsRec [] lis = lis
rmCharsRec (h:t) lis = rmCharsRec t (rmChar h lis)
--c)
rmCharsFold :: String -> String -> String
rmCharsFold lis c = foldr rmChar c lis

--8)
myReverse :: [a] -> [a]
myReverse lis = foldl (\xs x -> x:xs) [] lis

--9)
myElem :: Int -> [Int] -> Bool
myElem el lis = foldr (||) False (map (\x -> if el == x then True else False) lis)

--10)
myUnzip :: [(a, b)] -> ([a], [b])
myUnzip lis = foldr (\(x,y) (xs,ys) -> (x:xs, y:ys)) ([],[]) lis    

--11)
union ::[Int] -> [Int] -> [Int]
union lis1 lis2 = foldr (\x xs -> if myElem x xs then xs else x:xs) [] (lis1 ++ lis2)

--12)
intersect :: [Int] -> [Int] -> [Int]
intersect lis1 lis2 = foldr (\x xs -> if myElem x lis2 && myElem x xs == False  then x:xs else xs) [] lis1

--13)
mypermutations :: [a] -> [[a]]
mypermutations [] = [[]]
mypermutations (x:xs) = foldr (++) [] (map (insertOnAllPos [] x) (mypermutations xs))
    where
      insertOnAllPos :: [a] -> a -> [a] -> [[a]]
      insertOnAllPos xs x [] = [xs ++ [x]]
      insertOnAllPos xs x (y:ys) = (xs ++ (x:y:ys)) : (insertOnAllPos (xs ++ [y]) x ys)


