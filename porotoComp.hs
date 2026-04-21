--1)

data Nat = Zero | Succ Nat deriving Show

--a) Succ es un constructor de tipo inductivo

int2Nat :: Int -> Nat
int2Nat 0 = Zero
int2Nat x = Succ (int2Nat (x-1))

sumaN:: Nat -> Nat -> Nat
sumaN n1 Zero = n1
sumaN n1 (Succ n2) = (Succ (sumaN n1 n2))

nat2Int :: Nat -> Int
nat2Int Zero = 0
nat2Int (Succ n) = 1 + nat2Int n

--2)

data Arb = E | H Int | N Arb Arb deriving Show
data Cmd = L | R

--a) N es un constructor que se usa para unir dos subarboles y formar uno nuevo

selec :: [Cmd] -> Arb -> Arb
selec [] a = a
selec (L : cs) (N izq der) = selec cs izq
selec (R : cs) (N izq der) = selec cs der 








