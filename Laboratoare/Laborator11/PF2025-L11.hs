{-
class Functor f where
fmap : : ( a -> b ) -> f a -> f b
-}
newtype Identity a = Identity a

data Pair a = Pair a a

data Constant a b = Constant b

data Two a b = Two a b

data Three a b c = Three a b c

data Three' a b = Three' a b b


data Four a b c d = Four a b c d

data Four'' a b = Four'' a a a b

data Quant a b = Finance | Desk a | Bloor b


data LiftItOut f a = LiftItOut (f a)

data Parappa f g a = DaWrappa (f a) (g a)

data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

data Notorious g o a t = Notorious (g o) (g a) (g t)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

data TalkToMe a = Halt | Print String a | Read (String -> a)
--1)
instance Functor Identity where
  fmap f (Identity x) = Identity (f x)
--2)
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
--3)
instance Functor (Constant a) where
  fmap f (Constant b) = Constant (f b)
--4)
instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)
--5)
instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)
--6)
instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)
--7)
instance Functor (Four a b c) where
  fmap f (Four x y z w) = Four x y z (f w)
--8)
instance Functor (Four'' a) where
  fmap f (Four'' x y z w) = Four'' x y z (f w)
--9)
instance Functor (Quant a) where
  fmap f Finance = Finance
  fmap f (Desk x) = Desk x
  fmap f (Bloor y) = Bloor (f y)
--10)
instance Functor f => Functor (LiftItOut f) where
  fmap g (LiftItOut fa) = LiftItOut (fmap g fa)
--11)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa fa ga) = DaWrappa (fmap h fa) (fmap h ga)
--12)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething fa gb) = IgnoringSomething fa (fmap h gb)
--13)
instance Functor g => Functor (Notorious g o a) where
  fmap h (Notorious go ga gt) = Notorious go ga (fmap h gt)
--14)

instance Functor GoatLord where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat x) = OneGoat (f x)
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)
--15)
instance Functor TalkToMe where
  fmap f Halt = Halt
  fmap f (Print s x) = Print s (f x)
  fmap f (Read g) = Read (fmap f g)
