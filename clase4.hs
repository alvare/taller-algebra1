-- 1
data N = Z | S N

instance Show N where
    show x = show (showN x)

showN :: N -> Int
showN Z = 0
showN (S n) = 1 + showN n

restar :: N -> N -> N
restar Z n = error "naturales nou have negatives"
restar n Z = n
restar (S n) (S m) = restar n m

-- 3
-- la verguenza de implementar filter
filtrar :: [a] -> (a -> Bool) -> [a]
filtrar [] _ = []
filtrar (x:xs) condicion | condicion x = x : filtrar xs condicion
filtrar (x:xs) condicion | otherwise = filtrar xs condicion

contarPositivos :: Num a => [a] -> Int
contarPositivos lista = length $ filtrar lista (>0)

-- 4
elementoEnLista :: Eq a => [a] -> a -> Bool
elementoEnLista [] _ = False
elementoEnLista [x] y = x == y
elementoEnLista (x:xs) y | x == y = True
elementoEnLista (x:xs) y | otherwise = elementoEnLista xs y

-- 7
vueltear :: [Integer] -> [Integer]
vueltear [] = []
vueltear (x:xs) = vueltear xs ++ [x]

palindrome :: [Integer] -> Bool
palindrome lista = lista == vueltear lista

palindrome' :: [Integer] -> Bool
palindrome' [] = True
palindrome' [x] = True
palindrome' (x:xs) | x == (last xs) = palindrome' (init xs)
                   | otherwise = False
