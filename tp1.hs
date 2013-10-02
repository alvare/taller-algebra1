sumatoria :: (Num a) => (Int -> a) -> Int -> Int -> a
sumatoria f i n | i > n = 0
sumatoria f i n | i == n = f n
sumatoria f i n = f n + (sumatoria f i (pred n))

-- 1
funcionInterna1 :: Int -> Int
funcionInterna1 x = (2*x - 1)^2

punto1 :: Int -> Int
punto1 n = sumatoria funcionInterna1 1 n

-- 4
triangular :: [Int] -> Bool
triangular xs = creciente izq && decreciente der
    where izq = takeWhile (< mayor) xs
          der = dropWhile (< mayor) xs
          mayor = maximum xs

creciente :: [Int] -> Bool
creciente [] = True
creciente [x] = True
creciente (x:y:xs) = x <= y && creciente (y : xs)

decreciente :: [Int] -> Bool
decreciente [] = True
decreciente [x] = True
decreciente (x:y:xs) = x >= y && decreciente (y : xs)
