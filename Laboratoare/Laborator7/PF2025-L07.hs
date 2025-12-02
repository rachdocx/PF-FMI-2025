data Expr = Const Int 
          | Expr :+: Expr 
          | Expr :*: Expr 
           deriving Eq

data Operation = Add | Mult deriving (Eq, Show)

data Tree = Lf Int 
          | Node Operation Tree Tree 
           deriving (Eq, Show)
           
instance Show Expr where
  show (Const x) = show x
  show (e1 :+: e2) = "(" ++ show e1 ++ " + "++ show e2 ++ ")"
  show (e1 :*: e2) = "(" ++ show e1 ++ " * "++ show e2 ++ ")"           
-- 1)
evalExp :: Expr -> Int
evalExp (Const x) = x
evalExp (e1 :+: e2) = evalExp e1 + evalExp e2
evalExp (e1 :*: e2) = evalExp e1 * evalExp e2

exp1 = ((Const 2 :*: Const 3) :+: (Const 0 :*: Const 5))
exp2 = (Const 2 :*: (Const 3 :+: Const 4))
exp3 = (Const 4 :+: (Const 3 :*: Const 3))
exp4 = (((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2)
test11 = evalExp exp1 == 6
test12 = evalExp exp2 == 14
test13 = evalExp exp3 == 13
test14 = evalExp exp4 == 16
-- 2)
evalArb :: Tree -> Int
evalArb (Lf x) = x
evalArb (Node Add left right) = evalArb left + evalArb right
evalArb (Node Mult left right) = evalArb left * evalArb right


arb1 = Node Add (Node Mult (Lf 2) (Lf 3)) (Node Mult (Lf 0)(Lf 5))
arb2 = Node Mult (Lf 2) (Node Add (Lf 3)(Lf 4))
arb3 = Node Add (Lf 4) (Node Mult (Lf 3)(Lf 3))
arb4 = Node Mult (Node Mult (Node Mult (Lf 1) (Lf 2)) (Node Add (Lf 3)(Lf 1))) (Lf 2)

test21 = evalArb arb1 == 6
test22 = evalArb arb2 == 14
test23 = evalArb arb3 == 13
test24 = evalArb arb4 == 16

-- 3)
expToArb :: Expr -> Tree
expToArb (Const x) = Lf x
expToArb (e1 :+: e2) = Node Add (expToArb e1) (expToArb e2)
expToArb (e1 :*: e2) = Node Mult (expToArb e1) (expToArb e2)


data IntSearchTree value
  = Empty
  | BNode
      (IntSearchTree value)     -- elemente cu cheia mai mica
      Int                       -- cheia elementului
      (Maybe value)             -- valoarea elementului
      (IntSearchTree value)     -- elemente cu cheia mai mare

--4
lookup' :: Int -> IntSearchTree value -> Maybe value
lookup' _ Empty = Nothing
lookup' key (BNode left k val right)
  | key == k  = val
  | key < k   = lookup' key left
  | otherwise = lookup' key right
--5
keys ::  IntSearchTree value -> [Int]
keys Empty = []
keys (BNode left k _ right) = keys left ++ [k] ++ keys right
--6
values :: IntSearchTree value -> [value]
values Empty = []
values (BNode left _ Nothing right) = values left ++ values right
values (BNode left _ (Just v) right) = values left ++ [v] ++ values right
--7
insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert key val Empty = BNode Empty key (Just val) Empty
insert key val (BNode left k oldVal right)
  | key == k  = BNode left k (Just val) right
  | key < k   = BNode (insert key val left) k (Just val) right
  | otherwise = BNode left k (Just val) (insert key val right)
--8
delete :: Int -> IntSearchTree value -> IntSearchTree value
delete _ Empty = Empty
delete key (BNode left k val right)
  | key == k  = BNode left k Nothing right
  | key < k   = BNode (delete key left) k val right
  | otherwise = BNode left k val (delete key right)
--9
toList :: IntSearchTree value -> [(Int, value)]
toList Empty = []
toList (BNode left k Nothing right) = toList left ++ toList right
toList (BNode left k (Just v) right) = toList left ++ [(k, v)] ++ toList right
--10
fromList :: [(Int,value)] -> IntSearchTree value 
fromList = foldr (\(k, v) acc -> insert k v acc) Empty
--11
printTree :: IntSearchTree value -> String
printTree Empty = "."
printTree (BNode left k _ right) = "(" ++ printTree left ++ ") " ++ show k ++ " (" ++ printTree right ++ ")"
--12 extra
balance :: IntSearchTree value -> IntSearchTree value
balance tree = buildBalanced (toList tree)
  where
    buildBalanced [] = Empty
    buildBalanced lst = 
      let mid = length lst `div` 2
          (left, (k, v):right) = splitAt mid lst
      in BNode (buildBalanced left) k (Just v) (buildBalanced right)
