--1

borrarUltimo :: [a] -> [a]
borrarUltimo [] = error "papo"
borrarUltimo [x] = []
borrarUltimo (x:xs) = x : borrarUltimo xs


collect :: Eq k => [(k, v)] -> [(k, [v])]
collect = foldr insertar []
  where
    insertar (k, v) [] = [(k, [v])]
    insertar (k, v) ((k', vs):xs)
        | k == k'   = (k', v : vs) : xs
        | otherwise = (k', vs) : insertar (k, v) xs

serie :: [a] -> [[a]]
serie [] = [[]]
serie xs = serie (borrarUltimo xs) ++ [xs]

paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d 
  | a == b && c == d = True
  | a == d && b == c = True
  | a == c && b == d = True
  | otherwise = False
  

--f

ror :: Int -> [a] -> [a]
ror _ []=[]
ror 0 xs = xs
ror n (x:xs) = ror (n-1) (xs ++ [x])

ror' n [] resultado = ror' (rev resultado) []
ror' 0 xs resultado = xs ++ (rev resultado)
ror' n (x:xs) resultado = ror (n-1) xs (x: resultado) 

ror'' :: Int -> [a] -> [a] ->[a]-> [a]
ror'' n [] orig resultado =  ror'' n orig  orig []
ror'' 0 xs orig resultado = xs ++ rev resultado
ror'' n (x:xs) orig resultado = ror'' (n-1) xs orig (x:resultado)

--2
--c

divisores m = [ ]
--3

--foo f x = ((f 3)) == 3) && x

--4b

--foo2::  (b->c) -> (a -> b ) -> a -> c 

