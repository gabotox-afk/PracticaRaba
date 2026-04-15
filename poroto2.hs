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