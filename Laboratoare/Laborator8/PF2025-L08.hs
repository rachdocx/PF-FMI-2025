class Collection c where
  empty :: c key value
  singleton :: key -> value -> c key value
  insert
      :: Ord key
      => key -> value -> c key value -> c key value
  clookup :: Ord key => key -> c key value -> Maybe value
  delete :: Ord key => key -> c key value -> c key value
  keys :: c key value -> [key]
  values :: c key value -> [value]
  toList :: c key value -> [(key, value)]
  fromList :: Ord key => [(key,value)] -> c key value
  
  keys col = map fst (toList col)
  values col = map snd (toList col)
  fromList = foldr (\(k, v) acc -> insert k v acc) empty

newtype PairList k v
  = PairList { getPairList :: [(k, v)] }

instance Collection PairList where
  empty = PairList []
  
  singleton k v = PairList [(k, v)]
  
  insert k v (PairList pairs) = PairList (insertPair k v pairs)
    where
      insertPair k v [] = [(k, v)]
      insertPair k v ((k', v'):rest)
        | k == k'   = (k, v) : rest
        | otherwise = (k', v') : insertPair k v rest
  
  clookup k (PairList pairs) = lookup k pairs
  
  delete k (PairList pairs) = PairList (filter (\(k', _) -> k /= k') pairs)
  
  toList (PairList pairs) = pairs

data SearchTree key value
  = Empty
  | BNode
      (SearchTree key value) 
      key                  
      (Maybe value)         
      (SearchTree key value) 

instance Collection SearchTree where
  empty = Empty
  
  singleton k v = BNode Empty k (Just v) Empty
  
  insert k v Empty = BNode Empty k (Just v) Empty
  insert k v (BNode left key val right)
    | k == key  = BNode left key (Just v) right
    | k < key   = BNode (insert k v left) key val right
    | otherwise = BNode left key val (insert k v right)
  
  clookup k Empty = Nothing
  clookup k (BNode left key val right)
    | k == key  = val
    | k < key   = clookup k left
    | otherwise = clookup k right
  
  delete k Empty = Empty
  delete k (BNode left key val right)
    | k == key  = BNode left key Nothing right
    | k < key   = BNode (delete k left) key val right
    | otherwise = BNode left key val (delete k right)
  
  toList Empty = []
  toList (BNode left key Nothing right) = toList left ++ toList right
  toList (BNode left key (Just v) right) = toList left ++ [(key, v)] ++ toList right

data Punct = Pt [Int]

instance Show Punct where
  show (Pt []) = "()"
  show (Pt xs) = "(" ++ showCoords xs ++ ")"
    where
      showCoords [] = ""
      showCoords [x] = show x
      showCoords (x:xs) = show x ++ ", " ++ showCoords xs

data Arb = Vid | F Int | N Arb Arb
          deriving Show

class ToFromArb a where
  toArb :: a -> Arb
  fromArb :: Arb -> a

instance ToFromArb Punct where
  toArb (Pt []) = Vid
  toArb (Pt (x:xs)) = N (F x) (toArb (Pt xs))
  
  fromArb Vid = Pt []
  fromArb (F x) = Pt [x]
  fromArb (N left right) = 
    let Pt leftList = fromArb left
        Pt rightList = fromArb right
    in Pt (leftList ++ rightList)

data Geo a = Square a | Rectangle a a | Circle a
    deriving Show

class GeoOps g where
  perimeter :: (Floating a) => g a -> a
  area :: (Floating a) =>  g a -> a

instance GeoOps Geo where
  perimeter (Square l) = 4 * l
  perimeter (Rectangle l w) = 2 * (l + w)
  perimeter (Circle r) = 2 * pi * r
  
  area (Square l) = l * l
  area (Rectangle l w) = l * w
  area (Circle r) = pi * r * r

instance (Floating a, Eq a) => Eq (Geo a) where
  g1 == g2 = perimeter g1 == perimeter g2

