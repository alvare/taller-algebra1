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
