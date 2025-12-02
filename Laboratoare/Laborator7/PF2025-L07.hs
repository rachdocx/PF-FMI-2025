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

exp1 = (Const 2 :*: Const 3) :+: (Const 0 :*: Const 5)
exp2 = Const 2 :*: (Const 3 :+: Const 4)
exp3 = Const 4 :+: (Const 3 :*: Const 3)
exp4 = ((Const 1 :*: Const 2) :*: (Const 3 :+: Const 1)) :*: Const 2
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
lookup' cheie (BNode stanga k val dreapta)
  | cheie == k  = val
  | cheie < k   = lookup' cheie stanga
  | otherwise = lookup' cheie dreapta
--5
keys ::  IntSearchTree value -> [Int]
keys Empty = []
keys (BNode stanga k _ dreapta) = keys stanga ++ [k] ++ keys dreapta
--6
values :: IntSearchTree value -> [value]
values Empty = []
values (BNode stanga _ Nothing dreapta) = values stanga ++ values dreapta
values (BNode stanga _ (Just v) dreapta) = values stanga ++ [v] ++ values dreapta
--7
insert :: Int -> value -> IntSearchTree value -> IntSearchTree value
insert cheie val Empty = BNode Empty cheie (Just val) Empty
insert cheie val (BNode stanga k valVeche dreapta)
  | cheie == k  = BNode stanga k (Just val) dreapta
  | cheie < k   = BNode (insert cheie val stanga) k (Just val) dreapta
  | otherwise = BNode stanga k (Just val) (insert cheie val dreapta)
--8
delete :: Int -> IntSearchTree value -> IntSearchTree value
delete _ Empty = Empty
delete cheie (BNode stanga k val dreapta)
  | cheie == k  = BNode stanga k Nothing dreapta
  | cheie < k   = BNode (delete cheie stanga) k val dreapta
  | otherwise = BNode stanga k val (delete cheie dreapta)
--9
toList :: IntSearchTree value -> [(Int, value)]
toList Empty = []
toList (BNode stanga k Nothing dreapta) = toList stanga ++ toList dreapta
toList (BNode stanga k (Just v) dreapta) = toList stanga ++ [(k, v)] ++ toList dreapta
--10
fromList :: [(Int,value)] -> IntSearchTree value 
fromList = foldr (\(k, v) acc -> insert k v acc) Empty
--11
printTree :: IntSearchTree value -> String
printTree Empty = "."
printTree (BNode stanga k _ dreapta) = "(" ++ printTree stanga ++ ") " ++ show k ++ " (" ++ printTree dreapta ++ ")"
--12 extra
balance :: IntSearchTree value -> IntSearchTree value
balance arbore = echilibrat (toList arbore)
  where
    echilibrat [] = Empty
    echilibrat lista = 
      let mijloc = length lista `div` 2
          (stanga, (k, v):dreapta) = splitAt mijloc lista
      in BNode (echilibrat stanga) k (Just v) (echilibrat dreapta)
