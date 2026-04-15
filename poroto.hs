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
  



--2
--c

divisores m = [ ]
--3

--foo f x = ((f 3)) == 3) && x

--4b

--foo2::  (b->c) -> (a -> b ) -> a -> c 