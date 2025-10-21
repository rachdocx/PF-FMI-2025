import Data.Char
import Data.List (permutations)

-- 1 
--a) verificare lungime para lista
verifL :: [Int] -> Bool
verifL xs = length xs `mod` 2 == 0
--b1) eliminare ultimele n elemente din lista
takefinal :: [Int] -> Int -> [Int]
takefinal li n  
    | n >= length li = li
    | otherwise = drop (length li - n) li
--b2)
takefinalGENERAL :: [a] -> Int -> [a]
takefinalGENERAL li n  
    | n >= length li = li
    | otherwise = drop (length li - n) li
--c)
remove :: [a] -> Int -> [a]
remove li n
    | n >= length li = li
    | n < 0 = li
    | otherwise = take n li ++ drop (n + 1) li


-- semiPareRec [0,2,1,7,8,56,17,18] == [0,1,4,28,9]
semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
 | even h    = h `div` 2 : t'
 | otherwise = t'
 where t' = semiPareRec t
-- 2
-- a)
myreplicate :: Int -> Int -> [Int]
myreplicate 0 _ = []
myreplicate n x = x : myreplicate (n - 1) x
-- b)
sumImp :: [Int] -> Int
sumImp [] = 0
sumImp (h:t)
    | h `mod` 2 == 1 = h + sumImp t
    | otherwise = sumImp t
-- c)    
totalLen :: [String] -> Int
totalLen [] = 0
totalLen (h:t)
    | h == [] = totalLen t
    | head h == 'A' = 1 + totalLen t
    | head h == 'a' = 1 + totalLen t
    | otherwise = totalLen t
 --3.
nrVocaleString :: String -> Int
nrVocaleString [] = 0
nrVocaleString (h:t)
    | h `elem` "aeiouAEIOU" = 1 + nrVocaleString t
    | otherwise = nrVocaleString t
nrVocale :: [String] -> Int
nrVocale [] = 0
nrVocale (h:t)
    | reverse h == h = nrVocaleString h + nrVocale t
    | otherwise = nrVocale t

-- nrVocale ["sos", "civic", "palton", "desen", "aerisirea"] == 9
--4.
f :: Int -> [Int] -> [Int]
f _ [] = []
f n (h:t)
    | h `mod` 2 == 0 = h : n : f n t
    | otherwise = h : f n t
-- f 3 [1,2,3,4,5,6] = [1,2,3,3,4,3,5,6,3]

semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]

-- divizori 4 == [1,2,4]
--5
divizori :: Int -> [Int]
divizori n = [x | x<-[1..n], n `mod` x == 0 ]
--6
listadiv :: [Int] -> [[Int]]
listadiv li = [divizori x | x<-li]
-- listadiv [1,4,6,8] == [[1],[1,2,4],[1,2,3,6],[1,2,4,8]]

--7
--a)
inIntervalRec :: Int -> Int -> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec a b (h:t)
    | h >= a && h <= b = h : inIntervalRec a b t
    | otherwise = inIntervalRec a b t
--b)
inIntervalComp :: Int -> Int -> [Int] -> [Int]
inIntervalComp a b li = [x | x<-li, x>=a, x<=b]
-- inInterval 5 10 [1..15] == [5,6,7,8,9,10]
-- inInterval 5 10 [1,3,5,2,8,-1] == [5,8]
--8
--a)
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
    | h > 0 = 1 + pozitiveRec t
    | otherwise = pozitiveRec t 
--b)
pozitiveComp :: [Int] -> Int
pozitiveComp ls = length [x | x<-ls, x > 0 ]
-- pozitive [0,1,-3,-2,8,-1,6] == 3
--9.
--a)
aux :: [Int] -> Int -> [Int]
aux [] _ = []
aux (h:t) i
    | odd h     = i : aux t (i+1)
    | otherwise = aux t (i+1)

pozitiiImpareRec :: [Int] -> [Int]
pozitiiImpareRec [] = []
pozitiiImpareRec li = aux li 0 
--b)
pozitiiImpareComp :: [Int] -> [Int]
pozitiiImpareComp li = [i | (i,x) <- zip [0.. length li] li, x `mod` 2 == 1] 
-- pozitiiImpare [0,1,-3,-2,8,-1,6,1] == [1,2,5,7]


--10
--a)
multDigitsRec :: [Char] -> Int
multDigitsRec [] = 1
multDigitsRec (h:t) 
    | isDigit h = digitToInt h * multDigitsRec t
    | otherwise = multDigitsRec t
--b)
multDigitsComp :: [Char] -> Int
multDigitsComp s = product [digitToInt x | x <-s, isDigit x]
-- multDigits "The time is 4:25" == 40
-- multDigits "No digits here!" == 1

--EXTRAS
--permutari :: [Int] -> [[Int]]
--permutari [] = [[]]
--permutari (x:xs)

