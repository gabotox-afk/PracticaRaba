--1)

data RGB = Blue | Green | Red deriving (Show, Eq)
data Color = RGB Int Int Int deriving (Show, Eq)

mezclar :: Color -> Color -> Color
mezclar (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)

--2)
type Cursor = Int
type Linea = (String, Cursor) 

vacia :: Linea
vacia = ("", 0)

moverIzq :: Linea -> Linea
moverIzq (s, c) = (s, max 0 (c - 1))

moverDer :: Linea -> Linea
moverDer (s, c) = (s, min (length s) (c + 1))

moverIni :: Linea -> Linea
moverIni (s, c) = (s, 0)

moverFin :: Linea -> Linea
moverFin (s, c) = (s, length s)

insertar :: Char -> Linea -> Linea
insertar ch (s, c) = (take c s ++ [ch] ++ drop c s, c + 1)

take :: Int -> String -> String
take 0 s = []
take _ [] = []
take a (x:xs) = x : take (a-1) xs

drop:: Int -> String -> String
drop 0 s = s
drop _ []= []
drop a (x:xs) = drop (a-1) xs

borrar :: Linea -> Linea
borrar (s, c)
  | c > 0     = (take (c - 1) s ++ drop c s, c - 1)
  | otherwise = (s, c)



--3)

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show, Eq)

headCL :: CList a -> a
headCL (CUnit x) = x
headCL (Consnoc x l y) = x

tailCL :: CList a -> CList a
tailCL (CUnit _) = EmptyCL
tailCL (Consnoc _ l _) = l

isEmptyCl:: CList a -> Bool
isEmptyCl EmptyCL = True
isEmptyCl _ = False

isCunit:: CList a -> Bool
isCunit (CUnit a ) = True
isCunit _ = False

reverseClist :: CList a -> CList a
reverseClist (CUnit x) = Cunit x
reverseClist (Consnoc p cl u) = (Consnoc u (reverseClist cl) u)

-- c

initCL :: CList a -> CList a
initCL (CUnit _) = EmptyCL
initCL (Consnoc h EmptyCL _) = CUnit h
initCL (Consnoc h m _) = Consnoc h (initCL m) (lastCL m)

snocCL :: CList a -> a -> CList a
snocCL EmptyCL x = CUnit x
snocCL (CUnit h) x = Consnoc h EmptyCL x
snocCL (Consnoc h m l) x = Consnoc h (snocCL m l) x

inits :: CList a -> CList (CList a)
inits EmptyCL = CUnit EmptyCL
inits xs = snocCL (inits (initCL xs)) xs

-- d
lasts :: CList a -> CList (CList a)
lasts EmptyCL = CUnit EmptyCL
lasts xs = snocCL (lasts (tailCL xs)) xs

-- e
consCL :: a -> CList a -> CList a
consCL x EmptyCL = CUnit x
consCL x (CUnit y) = Consnoc x EmptyCL y
consCL x (Consnoc h m l) = Consnoc x (consCL h m) l

appendCL :: CList a -> CList a -> CList a
appendCL EmptyCL ys = ys
appendCL xs EmptyCL = xs
appendCL (CUnit x) ys = consCL x ys
appendCL (Consnoc h m l) ys = consCL h (appendCL (snocCL m l) ys)

concatCL :: CList (CList a) -> CList a
concatCL EmptyCL = EmptyCL
concatCL (CUnit xs) = xs
concatCL (Consnoc h m l) = appendCL h (appendCL (concatCL m) l)

--4
data Aexp = Num Int | Prod Aexp Aexp | Div Aexp Aexp

eval :: Aexp -> Int
eval (Num x) = x
eval (Prod x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y

seval :: Aexp -> Maybe Int
seval (Num x) = Just x
seval (Prod x y) = do
    a <- seval x
    b <- seval y
    return (a * b)
seval (Div x y) = do
    a <- seval x
    b <- seval y
    if b == 0
        then Nothing
        else Just (a `div` b)

--5
data BST a = Hoja | Nodo (BST a) a (BST a) 

maximum:: BST a -> Maybe a
maximum Hoja = Nothing
maximum (Nodo i x Hoja) = Just x
maximum (Nodo i x d) = maximum d

b2l::Ord => BST a -> [a]
b2l Hoja = []
b2l (Nodo i x d) = b2l i ++ [x] ++ b2l d

ordd:: Ord a =>[a] -> Bool
ordd [] = True
ordd [_] = True
ordd (x:y:xs)
            | x < y = True && ordd (y:xs)
            | otherwise = False
            
esBST:: BST a -> Bool
esBST t= ordd(b2l t)

--6
data Tree a = Leaf | TNodo (Tree a) a (Tree a) 

completo :: a -> Int -> Tree a
completo x 0 = Leaf
completo x 1 = TNodo Leaf x Leaf
completo x d = TNodo (completo x (d-1)) x (completo x (d-1))

balanceado :: a -> Int -> Tree a
balanceado x 0 = Leaf
balanceado x d = let m = (n-1 )`div` 2
                     sub = balanceado x 
                 in if (n-1) `mod` 2 == 0
                    then TNodo sub x sub
                    else TNodo sub x (balanceado x (m+1))

--7

member:: Int -> BST Int -> Bool
member x (Nodo i v d)
                    |x == v = True
                    |x < v = member x i
                    |x > v = member x d
                    |otherwise = False

memberBST :: Ord a => a -> BST a -> Bool
memberBST _ Hoja = False
memberBST a (Nodo l b r)
    | a == b    = True           -- 1 comparación
    | a < b     = memberBST a l -- 1 comparación + recursion
    | otherwise = memberBST a r -- 1 comparación + recursion


--8
data Color2 = R | B deriving show
data RBT a = E | N Color2 (RBT a) a (RBT a) deriving show

formordlist:: [a] -> RBT a 
formordlist xs = let

from' :: Color2 -> [a] -> RBT a
from'  c [] = E
from' c xs = let n = length xs
                |m = div n 2
                |ls = take m xs
                |x = xs !! m
                |rs = drop (m+1) xs
                |c' = if (c ==R) then B else r
              in N c (from' c' ls) x ( from' c' rs)


--10 

type Rank = Int
data Heap a = E | N Rank a (Heap a) (Heap a)

fromlist:: [a] -> Heap a
fromlist xs = foldr merge E (map (\x -> N 0 x E E) xs)

