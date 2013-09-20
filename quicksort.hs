quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = (quicksort $ filter (>x) xs) ++ [x] ++ (quicksort $ filter (<=x) xs)
