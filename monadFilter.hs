data Lista a = Nada | Agregar a (Lista a)

instance Monad Lista where
    return x = Agregar x Nada
    (>>=) Nada f = Nada
    (>>=) (Agregar x xs) f = concatenar (f x) (xs >>= f)
    fail _ = Nada

instance (Show a) => Show (Lista a) where
    show Nada = ""
    show (Agregar x Nada) = show x
    show (Agregar x y) = show x ++ ", " ++ show y

mapear :: Lista a -> (a -> b) -> Lista b
mapear Nada _ = Nada
mapear (Agregar x xs) f = Agregar (f x) $ mapear xs f

concatenar :: Lista a -> Lista a -> Lista a
concatenar Nada x = x
concatenar (Agregar x xs) ys = Agregar x (concatenar xs ys)

largo :: Lista a -> Int
largo Nada = 0
largo (Agregar x xs) = 1 + (largo xs)

hacerLista :: [a] -> Lista a
hacerLista [] = Nada
hacerLista (x:xs) = Agregar x (hacerLista xs)

filtrarPositivos :: (Num a, Ord a) => Lista a -> Lista a
filtrarPositivos lista = do
                           n <- lista
                           True <- return (n > 0)
                           return n

cantidadDePositivos  :: (Num a, Ord a) => Lista a -> Int
cantidadDePositivos lista = largo (filtrarPositivos lista)

cantPosArray :: [Int] -> Int
cantPosArray array = cantidadDePositivos (hacerLista array)
