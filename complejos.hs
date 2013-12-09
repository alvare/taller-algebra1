-- Ezequiel Alvarez (421/13) - Virginia Napoli (418/13) - Turno Noche
module Complejos (crearC, elevarC) where

data Complejo = C Float Float | E Float Float

instance Show Complejo where
    show (C a b) = show a ++ " + " ++ show b ++ "i"
    show (E r p) = show r ++ "*e^" ++ show p ++ "i"

crearC :: Float -> Float -> Complejo
crearC a b = C a b

modulo :: Complejo -> Float
modulo (C a b) = sqrt(a^2 + b^2)

arg :: Complejo -> Float
arg (C a b) | a == 0 && b > 0 = pi/2
            | a == 0 && b < 0 = (-1) * pi/2
            | a > 0 = atan(b/a)
            | a < 0 && b >= 0 = atan(b/a) + pi
            | a < 0 && b < 0 = atan(b/a) - pi

binomialAPolar :: Complejo -> Complejo
binomialAPolar c = E (modulo c) (arg c)

polarABinomial :: Complejo -> Complejo
polarABinomial (E r p) = C a b
  where a = (r * (cos p))
        b = (r * (sin p))

sacarVueltas :: Float -> Float
sacarVueltas x | x > 2 * pi = sacarVueltas (x - 2 * pi)
               | x < 0 = sacarVueltas (x + 2 * pi)
               | otherwise = x

elevarE :: Complejo -> Int -> Complejo
elevarE (E r p) n = E (r^n) (sacarVueltas (p * (fromIntegral n)))

elevarC :: Complejo -> Int -> Complejo
elevarC c n = polarABinomial formaPolarElevada
    where formaPolarElevada = elevarE (binomialAPolar c) n
