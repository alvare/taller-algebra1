decreciente :: Ord a => [a] -> Bool
decreciente [] = True
decreciente [x] = True
decreciente (x:y:xs) = x <= y && decreciente (y : xs)

repetido :: Eq a => [a] -> Bool
repetido [] = False
repetido (x:xs) = elem x xs || repetido xs

repetido' :: Eq a => [a] -> Bool
repetido' [] = False
repetido' (x:xs) = (foldl (||) False (map (\y -> y == x) xs)) || repetido' xs

-- 1
par :: Integral a => a -> Bool
par x = mod x 2 == 0

impar :: Integral a => a -> Bool
impar x = not $ par x

dec n = n - 1
inc n = n + 1

imparesHastaN :: Int -> [Int]
imparesHastaN 0 = []
imparesHastaN n | impar n = n : (imparesHastaN $ dec n)
imparesHastaN n | par n = imparesHastaN $ dec n

-- 2
listaGauss :: Int -> [Int]
listaGauss n = gaussIter n 1 1

gaussIter :: Int -> Int -> Int -> [Int]
gaussIter n sum count | count < n = sum : (gaussIter n next (inc count))
    where next = sum + (inc count)
gaussIter n sum count = [sum]

-- 3
ordenarCrecientes :: (Ord a) => [a] -> [a] -> [a]
ordenarCrecientes xxs [] = xxs
ordenarCrecientes [] yys = yys
ordenarCrecientes (x:xs) (y:ys) | x <= y = x : (ordenarCrecientes xs (y : ys))
ordenarCrecientes (x:xs) (y:ys) | otherwise = y : (ordenarCrecientes (x : xs) ys)
