{- 
class Functor f where 
     fmap :: (a -> b) -> f a -> f b 
class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

Just length <*> Just "world"

Just (++" world") <*> Just "hello,"
pure (+) <*> Just 3 <*> Just 5
pure (+) <*> Just 3 <*> Nothing
(++) <$> ["ha","heh"] <*> ["?","!"]
-}
data List a = Nil
            | Cons a (List a)
        deriving (Eq, Show)

--Ex 1
instance Functor List where
    fmap f Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons f fs) <*> xs = concatList (fmap f xs) (fs <*> xs)
      where
        concatList Nil ys = ys
        concatList (Cons x xs) ys = Cons x (concatList xs ys)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
test1 = (f <*> v) == Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

data Dog = Dog {
        name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

--Ex 2a
noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty s = Just s

noNegative :: Int -> Maybe Int
noNegative n 
  | n < 0 = Nothing
  | otherwise = Just n

test21 = noEmpty "abc" == Just "abc"
test22 = noNegative (-5) == Nothing 
test23 = noNegative 5 == Just 5 

--Ex 2b
dogFromString :: String -> Int -> Int -> Maybe Dog
dogFromString n a w = 
  case noEmpty n of
    Nothing -> Nothing
    Just validName -> case noNegative a of
      Nothing -> Nothing
      Just validAge -> case noNegative w of
        Nothing -> Nothing
        Just validWeight -> Just (Dog validName validAge validWeight)

--Ex 2c (usando fmap și <*>)
dogFromString' :: String -> Int -> Int -> Maybe Dog
dogFromString' n a w = Dog <$> noEmpty n <*> noNegative a <*> noNegative w 

test24 = dogFromString "Toto" 5 11 == Just (Dog {name = "Toto", age = 5, weight = 11})

newtype Name = Name String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person Name Address
    deriving (Eq, Show)

--Ex 3a
validateLength :: Int -> String -> Maybe String
validateLength maxLen s 
  | length s < maxLen = Just s
  | otherwise = Nothing

test31 = validateLength 5 "abc" == Just "abc"

--Ex 3b
mkName :: String -> Maybe Name
mkName s = case validateLength 25 s of
  Nothing -> Nothing
  Just validStr -> Just (Name validStr)

mkAddress :: String -> Maybe Address
mkAddress s = case validateLength 100 s of
  Nothing -> Nothing
  Just validStr -> Just (Address validStr)

test32 = mkName "Popescu" ==  Just (Name "Popescu")
test33 = mkAddress "Str Academiei" ==  Just (Address "Str Academiei")

--Ex 3c
mkPerson :: String -> String -> Maybe Person
mkPerson n a = case mkName n of
  Nothing -> Nothing
  Just validName -> case mkAddress a of
    Nothing -> Nothing
    Just validAddress -> Just (Person validName validAddress)

--Ex 3d (usando fmap și <*>)
mkName' :: String -> Maybe Name
mkName' s = Name <$> validateLength 25 s

mkAddress' :: String -> Maybe Address
mkAddress' s = Address <$> validateLength 100 s

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = Person <$> mkName' n <*> mkAddress' a 

test34 = mkPerson "Popescu" "Str Academiei" == Just (Person (Name "Popescu") (Address "Str Academiei"))
