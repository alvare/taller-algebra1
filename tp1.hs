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
