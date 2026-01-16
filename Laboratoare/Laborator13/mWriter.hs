
--- Monada Writer

newtype WriterS a = Writer { runWriter :: (a, String) } 


instance  Monad WriterS where
  return va = Writer (va, "")
  ma >>= k = let (va, log1) = runWriter ma
                 (vb, log2) = runWriter (k va)
             in  Writer (vb, log1 ++ log2)


instance  Applicative WriterS where
  pure = return
  mf <*> ma = do
    f <- mf
    a <- ma
    return (f a)       

instance  Functor WriterS where              
  fmap f ma = pure f <*> ma     

tell :: String -> WriterS () 
tell log = Writer ((), log)

--5a
logIncrement :: Int  -> WriterS Int
logIncrement x = do
  tell ("increment:" ++ show x ++ "\n")
  return (x + 1)

logIncrement2 :: Int -> WriterS Int
logIncrement2 x = do
  a <- logIncrement x
  logIncrement a

--b
logIncrementN :: Int -> Int -> WriterS Int
logIncrementN x 0 = return x
logIncrementN x n = do
  a <- logIncrement x
  logIncrementN a (n - 1)
   
                  

