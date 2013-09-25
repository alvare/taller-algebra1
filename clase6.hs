-- 1
quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = (quicksort menores) ++ [x] ++ (quicksort mayores)
    where menores = filter (<x) xs
          mayores = filter (>=x) xs
