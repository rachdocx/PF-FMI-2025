--- Monada Writer cu lista de mesaje (Ex 5c)

newtype WriterLS a = Writer { runWriter :: (a, [String]) } 


instance  Monad WriterLS where
  return va = Writer (va, [])
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterLS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterLS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterLS () 
tell log = Writer ((), [log])

--Ex 5c: logIncrement cu WriterLS
logIncrement :: Int  -> WriterLS Int
logIncrement x = do
  tell ("increment:" ++ show x)
  return (x + 1)

logIncrementN :: Int -> Int -> WriterLS Int
logIncrementN x 0 = return x
logIncrementN x n = do
  a <- logIncrement x
  logIncrementN a (n - 1)

{-
runWriter $ logIncrementN 2 4
(6,["increment:2","increment:3","increment:4","increment:5"])
-}
