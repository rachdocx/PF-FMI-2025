myInt = 31415926535897932384626433832795028841971693993751058209749445923

double :: Integer -> Integer
double x = x+x

maxim :: Integer -> Integer -> Integer
maxim x y = if (x > y)
               then x
          else y

max3 x y z = let
             u = maxim x y
             in (maxim  u z)


--maximul dintre 4 cu let si indendatri
maxim4 x y z a = 
    let 
        max1 = maxim x y
        max2 = maxim z a
    in
        maxim max1 max2 

--maximul dintre 3
maxim3 x y z =
    if x >= y then
        if x >= z then x
        else z
    else
        if y >= z then y
        else z


-- verificare max4
verMax4 x y z a =
    let
        max1 = maxim4 x y z a
    in
        max1 >= a && max1 >= z && max1 >=x && max1 >= y

--6
--a) o funcție cu doi parametri care calculează suma pătratelor lor;
sumaPatrate a b = 
    a*a + b*b
--b) o funcție cu un parametru ce întoarce stringul "par" dacă parametrul este par și "impar" altfel;
ePar a = 
    if a `mod` 2 == 0
        then "par"
    else
        "impar"
--c) o funcție care calculează factorialul unui număr;

factorial x =
    if x == 0 || x == 1
        then 1
    else
        x * factorial (x - 1) 
--d) o funcție care verifică dacă primul parametru este mai mare decât dublul celui de-al doilea parametru; 
isGreater a b =
    if a > b * 2
        then "true"
    else
        "false"

--e) o funcție care calculează elementul maxim al unei liste.

maximList [x] = x
maximList (x:xs) = 
    let
        m = maximList xs
    in
        if x > m 
            then x
        else
            m
--poly 7
poly :: Double -> Double -> Double -> Double -> Double
poly a b c x = 
    a * x^2 + b * x + c
--8
eeny :: Integer -> String
eeny a =     
    if a `mod` 2 == 0
        then "par"
    else
        "impar"
--9
fizzbuzz :: Integer -> String
fizzbuzz a= 
    if a `mod` 15 == 0
        then "FizzBuzz"
    else
        if a `mod` 5 == 0
            then "Buzz"
        else
            if a `mod` 3 == 0
                then "Fizz"
            else
                []
-- garzi
fizzbuzz' x
    | x `mod` 15 == 0 = "FizzBuzz"
    | x `mod` 3  == 0 = "Fizz"
    | x `mod` 5  == 0 = "Buzz"
    | otherwise        = ""
fibonacciCazuri :: Integer -> Integer
fibonacciCazuri n
    | n < 2     = n
    | otherwise = fibonacciCazuri (n - 1) + fibonacciCazuri (n - 2)

fibonacciEcuational :: Integer -> Integer
fibonacciEcuational 0 = 0
fibonacciEcuational 1 = 1
fibonacciEcuational n =
    fibonacciEcuational (n - 1) + fibonacciEcuational (n - 2)
-- 10
tribonacci :: Integer -> Integer
tribonacci 0 = 0
tribonacci 1 = 1
tribonacci 2 = 1
tribonacci n =
    tribonacci (n-1) + tribonacci(n-2) + tribonacci(n-3)

tribonaccicazuri n 
    | n == 0    = 0
    | n == 1    = 1
    | n == 2    = 2
    | otherwise = tribonaccicazuri (n-1) + tribonaccicazuri(n-2) + tribonaccicazuri(n-3)
binomial :: Integer -> Integer -> Integer
binomial n k 
    | k == 0 && n >=0 = 1
    | n == 0 && k>=0 = 0
    | otherwise = binomial(n-1) (k) + binomial(n-1) (k-1)
