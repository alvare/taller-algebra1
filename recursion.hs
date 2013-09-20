fibarr :: Integer -> Integer -> [Integer]
fibarr a b = b : fibarr b (a + b)

fib' :: Int -> Integer
fib' n = last $ take n $ fibarr 0 1

factorial :: Integer -> Integer
factorial 0 = 1
factorial x | x > 0 = x * factorial (x - 1)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x | x > 1 = fib (x - 1) + fib (x - 2)

sumaImpares :: Integer -> Integer
sumaImpares 0 = 0
sumaImpares x | x `mod` 2 /= 0 = x + sumaImpares (x - 1)
sumaImpares x = sumaImpares (x - 1)
