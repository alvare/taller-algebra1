-- Ezequiel Alvarez (421/13) - Virginia Napoli (418/13) - Turno Noche
-- utilidades
base10aEntero :: [Int] -> Int
base10aEntero [x] = x
base10aEntero (x:xs) = x*(10^indice) + base10aEntero xs
    where indice = length xs

divisiblePor :: Int -> Int -> (Int -> Int -> Int) -> Bool
divisiblePor dividendo x test | x == 0 = True
                              | x == dividendo = True
                              | x < dividendo = False
                              | otherwise = divisiblePor dividendo (abs (test cifras unidad)) test
                              where cifras = div x 10
                                    unidad = mod x 10

-- 1
divisible17 :: [Int] -> Bool
divisible17 xxs = divisiblePor 17 (base10aEntero xxs) test17
    where test17 restantes ultimo = restantes - (ultimo * 5)

-- 2
divisible19 :: [Int] -> Bool
divisible19 xxs = divisiblePor 19 (base10aEntero xxs) test19
    where test19 restantes ultimo = restantes + (ultimo * 2)

-- 3
criba :: [Int] -> [Int]
criba [] = []
criba (x:xs) = x : criba (filter noMultiploDeX xs)
    where noMultiploDeX m = mod m x /= 0

primosHasta :: Int -> [Int]
primosHasta n = criba [2..n]

sumaDePrimos :: Int -> [Int] -> Bool
sumaDePrimos _ [] = False
sumaDePrimos x (p:ps) = elem (x - p) (p:ps) || sumaDePrimos x ps

todosSonSuma :: [Int] -> [Int] -> Bool
todosSonSuma [] _ = True
todosSonSuma (x:xs) primos = sumaDePrimos x primos && todosSonSuma xs primos

goldbach :: Int -> Bool
goldbach n = todosSonSuma [4,6..n] (primosHasta n)

-- 4 complejos.hs

-- 5
mcd :: Integer -> Integer -> Integer
mcd a 0 = a
mcd a b = mcd b (mod a b)

tieneSolucion :: [Integer] -> Integer -> Bool
tieneSolucion [] _ = error "Debe ingresar al menos un coeficiente"
tieneSolucion [x] c = mod c x == 0
tieneSolucion (a:b:xs) c = tieneSolucion ((mcd a b) : xs) c
