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

--4
data Bin a = Hoja | Nodo (Bin a) a (Bin a)

nodosL :: Bin a -> Int -> Int
nodosL Hoja _ = 0
nodosL (Nodo izq _ der) 0 = 1  
nodosL (Nodo izq _ der) n = nodosL izq (n-1) + nodosL der (n-1) 

esBalanceado :: Bin a -> Int
esBalanceado Hoja = 0
esBalanceado (Nodo izq _ der) 
    | izqBalanceado == -1 = -1
    | derBalanceado == -1 = -1
    | abs(izqBalanceado - derBalanceado) <= 1 = 1 + max izqBalanceado derBalanceado
    | otherwise = -1
  where
    izqBalanceado = esBalanceado izq
    derBalanceado = esBalanceado der

sucepredece :: Ord a => Bin a -> a -> (Maybe a, Maybe a)
sucepredece Hoja _ = (Nothing, Nothing)
sucepredece (Nodo izq val der) x
    | x < val   = (Just val, predIzq)
    | x > val   = (succDer, Just val)
    | otherwise = (succesor der, predecesor izq)
  where
    succesor Hoja = Nothing
    succesor (Nodo Hoja v der') = Just v
    succesor (Nodo izq' _ _) = succesor izq'
    
    predecesor Hoja = Nothing
    predecesor (Nodo izq' v Hoja) = Just v
    predecesor (Nodo _ _ der') = predecesor der'
    
    predIzq = predecesor izq
    succDer = succesor der 