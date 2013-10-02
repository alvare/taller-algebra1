div' :: Int -> Int -> Int
div' n m | n < m = 0
div' n m | n >= m = 1 + div' (n - m) m

mod' :: Int -> Int -> Int
mod' n m | n < m = n
mod' n m | n >= m =  mod' (n - m) m

baseR :: Int -> Int -> [Int]
baseR 0 _ = []
baseR x r = (baseR (x `div` r) r) ++ [x `mod` r]
