{-
[x^2 |x <- [1..10], x `rem` 3 == 2]
Raspuns: [4,25,64,100]

[(x,y) | x <- [1..5], y <- [x..(x+2)]]
Raspuns: [(1,1), (1,2), (1,3), (2,2), (2,3), (2,4), (3,3), (3,4), (3,5), (4,4), (4,5), (4,6), (5,5), (5,6), (5,7)]

[(x,y) | x <- [1..3], let k = x^2, y <- [1..k]]
Raspuns: [(1,1),(2,1),(2,2),(3,1),(3,2),(3,3),(3,4),(3,5),(3,6),(3,7),(3,8),(3,9)]

[x | x <- "Facultatea de Matematica si Informatica", elem x ['A'..'Z']]
Raspuns: "FMI"

[[x..y] | x <- [1..5], y <- [1..5], x < y]
Raspuns: [[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5],[2,3],[2,3,4],[2,3,4,5],[3,4],[3,4,5],[4,5]]
-}

--2
factori :: Int -> [Int]
factori x = [d | d <- [1..x], x `mod` d == 0]

--3
prim :: Int -> Bool
prim x 
    | length (factori x) == 2 = True
    | otherwise = False

--4
numerePrime :: Int -> [Int]
numerePrime x = [a | a <- [2..x], prim a]

-- [(x,y)| x <- [1..5], y <- [1..3]]
--5 
myzip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myzip3 [] _ _ = []
myzip3 _ [] _ = []
myzip3 _ _ [] = []
myzip3 (ha:ta) (hb:tb) (hc:tc) = (ha,hb,hc) : myzip3 ta tb tc
-- 6
firstEl :: [(a, b)] -> [a]
firstEl = map (\(x,_) -> x)
 --7
sumList :: [[Int]] -> [Int]
sumList = map sum
--8
prel2 :: [Int] -> [Int]
prel2 = map (\x -> if even x then x `div` 2 else 2 * x)
--9
f9 :: Char -> [String] -> [String]
f9 c = filter (elem c)
--10
oddsquare :: [Int] -> [Int]
oddsquare xs = map (^2) (filter odd xs)
--11
squareoddpos :: [Int] -> [Int]
squareoddpos xs = map (\(_, v) -> v ^ 2) (filter (odd . fst) (zip [0..] xs))
--12
numaiVocale :: [String] -> [String]
numaiVocale xs = map (filter (\c -> c `elem` "aeiouAEIOU")) xs



--mymap
mymap :: (a -> b) -> [a] -> [b]
mymap f [] = []
mymap f (x:xs) = f x : mymap f xs 

--myfilter
myfilter :: (a -> Bool) -> [a] -> [a]
myfilter p [] = []
myfilter p (x:xs)
    | p x= x : myfilter p xs
    | otherwise = myfilter p xs











ordonataNat :: [Int] -> Bool
ordonataNat [] = True
ordonataNat [x] = True
ordonataNat (x:xs) = undefined
ordonata :: [a] -> (a -> a -> Bool) -> Bool
ordonata = undefined
