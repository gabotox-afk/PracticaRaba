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


enum :: Arb -> [[Cmd]]
enum  E  = []
enum (H x)  = [[]]
enum (N l r) = map (L:) (enum l) ++ map (R:) (enum r)


-- 3
type Nombre = String
data Estado a = Estado [(Nombre , a)] deriving show
data Maybe a = Nothing | Just a deriving Show

inicial :: Estado a
inicial = Estado []

update :: Nombre -> a -> Estado a -> Estado a
update n x (Estado []) = (Estado [(n , x)])
update n x (Estado ((n2 , y) : xs) )
                          | n == n2 = (Estado ( n2,x): xs)
                          | otherwise = update n x (Estado xs) 

lookfor :: Nombre -> Estado a -> Maybe a
lookfor n Estado [] = Nothing
lookfor n (Estado ((n2,x):xs))
                            | n == n2 =Just x
                            | otherwise = lookfor n (Estado xs)

free :: Nombre -> Estado a -> Estado a
free n (Estado []) = Estado []
free n (Estado ((n2,x):xs))
                        | n == n2 = Estado xs
                        | otherwise = Estado (n2,x ):(free n (Estado xs))




