
{- Monada Maybe este definita in GHC.Base 

instance Monad Maybe where
  return = Just
  Just va  >>= k   = k va
  Nothing >>= _   = Nothing


instance Applicative Maybe where
  pure = return
  mf <*> ma = do
    f <- mf
    va <- ma
    return (f va)       

instance Functor Maybe where              
  fmap f ma = pure f <*> ma   
-}

pos :: Int -> Bool
pos  x = if (x>=0) then True else False

fct :: Maybe Int ->  Maybe Bool
fct  mx =  mx  >>= (\x -> Just (pos x))

--Ex 1: fct cu notatie do
fct' :: Maybe Int -> Maybe Bool
fct' mx = do
  x <- mx
  return (pos x)

--Ex 2a: addM cu pattern matching
addM :: Maybe Int -> Maybe Int -> Maybe Int
addM (Just x) (Just y) = Just (x + y)
addM _ _ = Nothing

--Ex 2b: addM cu notatie do
addM' :: Maybe Int -> Maybe Int -> Maybe Int
addM' mx my = do
  x <- mx
  y <- my
  return (x + y)


cartesian_product xs ys = xs >>= ( \x -> (ys >>= \y-> return (x,y)))

--Ex 3a: cartesian_product cu notatie do
cartesian_product' xs ys = do
  x <- xs
  y <- ys
  return (x, y)

prod f xs ys = [f x y | x <- xs, y<-ys]

--Ex 3b: prod cu notatie do
prod' f xs ys = do
  x <- xs
  y <- ys
  return (f x y)

myGetLine :: IO String
myGetLine = getChar >>= \x ->
      if x == '\n' then
          return []
      else
          myGetLine >>= \xs -> return (x:xs)

--Ex 3c: myGetLine cu notatie do
myGetLine' :: IO String
myGetLine' = do
  x <- getChar
  if x == '\n' then
    return []
  else do
    xs <- myGetLine'
    return (x:xs)

prelNo noin =  sqrt noin

ioNumber = do
     noin  <- readLn :: IO Float
     putStrLn $ "Intrare\n" ++ (show noin)
     let  noout = prelNo noin
     putStrLn $ "Iesire"
     print noout

--Ex 4: ioNumber cu secventiere (>>=)
ioNumber' = 
  (readLn :: IO Float) >>= \noin ->
  putStrLn ("Intrare\n" ++ show noin) >>
  let noout = prelNo noin in
  putStrLn "Iesire" >>
  print noout

data Person = Person { name :: String, age :: Int }

--Ex 6a
showPersonN :: Person -> String
showPersonN p = "NAME: " ++ name p

showPersonA :: Person -> String
showPersonA p = "AGE: " ++ show (age p)

{-
showPersonN $ Person "ada" 20
"NAME: ada"
showPersonA $ Person "ada" 20
"AGE: 20"
-}

--Ex 6b
showPerson :: Person -> String
showPerson p = "(" ++ showPersonN p ++ ", " ++ showPersonA p ++ ")"

{-
showPerson $ Person "ada" 20
"(NAME: ada, AGE: 20)"
-}


newtype Reader env a = Reader { runReader :: env -> a }


instance Monad (Reader env) where
  return x = Reader (\_ -> x)
  ma >>= k = Reader f
    where f env = let a = runReader ma env
                  in  runReader (k a) env



instance Applicative (Reader env) where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance Functor (Reader env) where              
  fmap f ma = pure f <*> ma    

--Ex 6c: variante monadice cu Reader
mshowPersonN ::  Reader Person String
mshowPersonN = Reader $ \p -> "NAME:" ++ name p

mshowPersonA ::  Reader Person String
mshowPersonA = Reader $ \p -> "AGE:" ++ show (age p)

mshowPerson ::  Reader Person String
mshowPerson = do
  n <- mshowPersonN
  a <- mshowPersonA
  return $ "(" ++ n ++ "," ++ a ++ ")"

{-
runReader mshowPersonN  $ Person "ada" 20
"NAME:ada"
runReader mshowPersonA  $ Person "ada" 20
"AGE:20"
runReader mshowPerson  $ Person "ada" 20
"(NAME:ada,AGE:20)"
-}