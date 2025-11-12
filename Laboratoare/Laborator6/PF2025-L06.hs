data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe =
  [ Mar "Ionatan" False
  , Portocala "Sanguinello" 10
  , Portocala "Valencia" 22
  , Mar "Golden Delicious" True
  , Portocala "Sanguinello" 15
  , Portocala "Moro" 12
  , Portocala "Tarocco" 3
  , Portocala "Moro" 12
  , Portocala "Valencia" 2
  , Mar "Golden Delicious" False
  , Mar "Golden" False
  , Mar "Golden" True
  ]

-- 1a)
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Portocala s _) = s `elem` ["Tarocco", "Moro", "Sanguinello"]
ePortocalaDeSicilia _ = False

test_ePortocalaDeSicilia1 = ePortocalaDeSicilia (Portocala "Moro" 12)
test_ePortocalaDeSicilia2 = ePortocalaDeSicilia (Mar "Ionatan" True) == False

-- 1b)
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia fr = sum [n | Portocala s n <- fr, s `elem` ["Tarocco", "Moro", "Sanguinello"]]

test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

-- 1c)
nrMereViermi :: [Fruct] -> Int
nrMereViermi fr = length [areViermi | Mar soi areViermi <- fr, areViermi]

test_nrMereViermi = nrMereViermi listaFructe == 2

type NumeA = String
type Rasa = String

data Animal = Pisica NumeA | Caine NumeA Rasa
  deriving Show

-- 2a)
vorbeste :: Animal -> String
vorbeste (Pisica _) = "Meow!"
vorbeste (Caine _ _) = "Woof!"

-- 2b)
rasa :: Animal -> Maybe String
rasa (Caine _ r) = Just r
rasa (Pisica _) = Nothing

data Linie = L [Int]
  deriving Show

data Matrice = M [Linie]
  deriving Show

-- 3a)
verifica :: Matrice -> Int -> Bool
verifica (M linii) n = foldr (\(L x) acc -> sum x == n && acc) True linii

test_verif1 = verifica (M [L [1,2,3], L [4,5], L [2,3,6,8], L [8,5,3]]) 10 == False
test_verif2 = verifica (M [L [2,20,3], L [4,21], L [2,3,6,8,6], L [8,5,3,9]]) 25 == True

-- 3b)
doarPozN :: Matrice -> Int -> Bool
doarPozN (M linii) n = foldr (\(L x) acc -> (length x /= n || all (>0) x) && acc) True linii

testPoz1 = doarPozN (M [L [1,2,3], L [4,5], L [2,3,6,8], L [8,5,3]]) 3 == True
testPoz2 = doarPozN (M [L [1,2,-3], L [4,5], L [2,3,6,8], L [8,5,3]]) 3 == False

-- 3c)
corect :: Matrice -> Bool
corect (M []) = True
corect (M [L _]) = True
corect (M (L xs : rest)) = foldr (\(L y) acc -> length y == length xs && acc) True rest

testcorect1 = corect (M [L [1,2,3], L [4,5], L [2,3,6,8], L [8,5,3]]) == False
testcorect2 = corect (M [L [1,2,3], L [4,5,8], L [3,6,8], L [8,5,3]]) == True
