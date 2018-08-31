-- module for basic functions and algorithms of number theory
module NumberTheory where

-- determines whether a divides b
divides :: Integer -> Integer -> Bool
divides a b = (mod b a) == 0

-- computes the greatest common divisor (GCD) of a and b
-- see Euclidean algorithm: https://en.wikipedia.org/wiki/Euclidean_algorithm
euclid :: Integer -> Integer -> Integer
euclid a b =
    if (b == 0)
    then
        a
    else
        if (b > a)
        then
            euclid b a
        else
            euclid b (mod a b)

-- computes the greatest common divisor (GCD) of a and b
-- by using the euclidean algorithm
gcd a b = euclid a b

-- computes the lowest common multiple (LCM) of a and b
-- using GCD(a, b) * LCM(a, b) = a * b => LCM(a, b) = (a * b) / GCD(a, b)
lcm a b = div (a * b) (NumberTheory.gcd a b)

-- solves an equation of the form a * x + b * y = GCD(a, b) for x, y and d as the GCD(a, b) -> (d, x, y)
-- see extended Euclidean algorithm: https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm
extendedEuclid :: Integer -> Integer -> (Integer, Integer, Integer)
extendedEuclid a b =
    if (b == 0)
    then
        (a, 1, 0)
    else
        let (d, s, t) = extendedEuclid b (mod a b)
        in
            (d, t, s - (div a b) * t)

-- solves a linear Diophantine equation of the form a * x + b * y = c if and only if GCD(a, b) divides c
-- for x, y and d as the GCD(a, b) by using the extended Euclidean algorithm -> (d, x, y)
-- see Diophantine equation: https://en.wikipedia.org/wiki/Diophantine_equation
solveLinearDiophantine :: Integer -> Integer -> Integer -> (Integer, Integer, Integer)
solveLinearDiophantine a b c =
    if (divides d c)
    then
        (d, s * (div c d), t * (div c d))
    else
        error "No solution"
    where
        (d, s, t) = extendedEuclid a b

-- determines the modular multiplicative inverse d of a mod m, where a * d mod m = 1,
-- by solving the linear Diophantine equation a * d = q * m + 1 <=> a * d - q * m = 1 for d
modInverse :: Integer -> Integer -> Integer
modInverse a m = 
    mod s (div m d)
    where (d, s, t) = solveLinearDiophantine a m 1

-- calculates b ^ e mod m by using a variety of simplifications steps
-- to accelerate the process and prevent out of memory errors
modPow :: Integer -> Integer -> Integer -> Integer
modPow b e m =
    if (b == 1)
    then
        mod 1 m
    else if (b == -1)
    then
        if (divides 2 e)
        then
            mod 1 m
        else
            mod (-1) m
    else if (e >= 2)
    then
        if (divides 2 e)
        then
            modPow (mod (b * b) m) (div e 2) m
        else
            mod (b * (modPow b (e - 1) m)) m
    else
        mod (b ^ e) m

-- determines whether a and b are relatively prime
relativelyPrime :: Integer -> Integer -> Bool
relativelyPrime a b = (NumberTheory.gcd a b) == 1

-- tries to find a and b such that a * b mod m = 1 and a /= b -> (a, b)
findRelativelyPrimePair :: Integer -> (Integer, Integer)
findRelativelyPrimePair m =
    loop m
    where
        loop c =
            if (c < 1)
            then
                error "Unable to find relatively prime pair of m"
            else if (relativelyPrime c m)
            then
                let i = modInverse c m
                in
                    if (c /= i)
                    then
                        (c, i)
                    else
                        loop (c - 1)
            else
                loop (c - 1)
