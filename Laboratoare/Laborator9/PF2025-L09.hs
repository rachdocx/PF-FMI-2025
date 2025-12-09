data Tree = Empty  
   | Node Int Tree Tree Tree 
      
extree :: Tree
extree = Node 4 (Node 5 Empty Empty Empty) 
                (Node 3 Empty Empty (Node 1 Empty Empty Empty)) Empty

class ArbInfo t where
  level :: t -> Int 
                      
  sumval :: t -> Int 
  nrFrunze :: t -> Int 

--1
instance ArbInfo Tree where
  level Empty = 0
  level (Node _ t1 t2 t3) = 1 + maximum [level t1, level t2, level t3]

  
  sumval Empty = 0
  sumval (Node val t1 t2 t3) = val + sumval t1 + sumval t2 + sumval t3

  
  
  nrFrunze Empty = 0
  nrFrunze (Node _ Empty Empty Empty) = 1
  nrFrunze (Node _ t1 t2 t3) = nrFrunze t1 + nrFrunze t2 + nrFrunze t3


class Scalar a where
  zero :: a
  one :: a
  adds :: a -> a -> a
  mult :: a -> a -> a
  negates :: a -> a
  recips :: a -> a

--2
instance Scalar Int where
  zero = 0
  one = 1
  adds = (+)
  mult = (*)
  negates = negate
  recips x = if x /= 0 then 1 `div` x else error "impartire la zero"

instance Scalar Float where
  zero = 0.0
  one = 1.0
  adds = (+)
  mult = (*)
  negates = negate
  recips x = if x /= 0 then 1.0 / x else error "impartire la zero"

instance Scalar Double where
  zero = 0.0
  one = 1.0
  adds = (+)
  mult = (*)
  negates = negate
  recips x = if x /= 0 then 1.0 / x else error "impartire la zero"

class (Scalar a) => Vector v a where
  zerov :: v a
  onev :: v a
  addv :: v a -> v a -> v a 
  smult :: a -> v a -> v a  
  negatev :: v a -> v a 

data Vector2D a = V2D a a
  deriving (Show, Eq)

data Vector3D a = V3D a a a
  deriving (Show, Eq)

--3
instance (Scalar a) => Vector Vector2D a where
  zerov = V2D zero zero
  onev = V2D one one
  addv (V2D x1 y1) (V2D x2 y2) = V2D (adds x1 x2) (adds y1 y2)
  smult s (V2D x y) = V2D (mult s x) (mult s y)
  negatev (V2D x y) = V2D (negates x) (negates y)

instance (Scalar a) => Vector Vector3D a where
  zerov = V3D zero zero zero
  onev = V3D one one one
  addv (V3D x1 y1 z1) (V3D x2 y2 z2) = V3D (adds x1 x2) (adds y1 y2) (adds z1 z2)
  smult s (V3D x y z) = V3D (mult s x) (mult s y) (mult s z)
  negatev (V3D x y z) = V3D (negates x) (negates y) (negates z)

