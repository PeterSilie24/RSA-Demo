-- module for basic RSA cryptography
-- see RSA: https://en.wikipedia.org/wiki/RSA_(cryptosystem)
module RSA where

import NumberTheory

import Data.Char

-- combines a private and a public key to a key pair
rsaKeyPair :: (Integer, Integer) -> (Integer, Integer) -> ((Integer, Integer), (Integer, Integer))
rsaKeyPair (a, b) (c, d) = ((a, b), (c, d))

-- retrieves the private key from a key pair
rsaPrivateKey :: ((Integer, Integer), (Integer, Integer)) -> (Integer, Integer)
rsaPrivateKey ((a, b), (c, d)) = (a, b)

-- retrieves the public key from a key pair
rsaPublicKey :: ((Integer, Integer), (Integer, Integer)) -> (Integer, Integer)
rsaPublicKey ((a, b), (c, d)) = (c, d)

-- retrieves the modulus from a key
rsaModulus :: (Integer, Integer) -> Integer
rsaModulus (m, e) = m

-- retrieves the exponent from a key
rsaExponent :: (Integer, Integer) -> Integer
rsaExponent (m, e) = e

-- generates a RSA key pair consisting of a private and a public key
-- 1. choose to distinct primes p and q when using this function
-- 2. the RSA modulus n will be computed with n = p * q
-- 3. phi(n) will be computed with phi(n) = phi(p) * phi(q) = (p - 1) * (q - 1)
--    see Euler's totient function: https://en.wikipedia.org/wiki/Euler%27s_totient_function
-- 4. e will be chosen such that 1 < e < phi(n) and GCD(e, phi(n)) = 1
-- 5. d will be determined such that e * d mod phi(n) = 1
--    step 4 and 5 will be both done by findRelativelyPrimePair
-- the public key consists of the modulus n and the exponent e
-- the private key consists of the modulus n and the exponent d
rsaGenerateKeyPair :: Integer -> Integer -> ((Integer, Integer), (Integer, Integer))
rsaGenerateKeyPair p q =
    rsaKeyPair (n, d) (n, e)
    where
        n = p * q
        phiN = (p - 1) * (q - 1)
        (e, d) = findRelativelyPrimePair phiN

-- decrypts the message m from c with a private key (n, d)
-- where m mod n = c ^ d mod n
rsaDecrypt :: (Integer, Integer) -> Integer -> Integer
rsaDecrypt (n, d) c = modPow c d n

-- encrypts the message m to c with a public key (n, e)
-- where c mod n = m ^ e mod n
rsaEncrypt :: (Integer, Integer) -> Integer -> Integer
rsaEncrypt (n, e) m = modPow m e n

-- why this works:
--
-- a ^ phi(b) mod b = 1 mod n if a and b are coprime
-- see Euler's theorem: https://en.wikipedia.org/wiki/Euler%27s_theorem
--
-- e * d mod phi(n) = 1 => e * d = z * phi(n) + 1 where z is an integer
--
-- m mod n = c ^ d mod n
-- c mod n = m ^ e mod n
--
-- => (m ^ e) ^ d mod n
--  = m ^ (e * d) mod n 
--  = m ^ (z * phi(n) + 1) mod n
--  = m ^ (z * phi(n)) * m mod n
--  = (m ^ phi(n)) ^ z * m mod n
--  = 1 ^ z * m mod n (Euler's theorem)
--  = 1 * m mod n
--  = m mod n
--
-- (Remark: for Euler's theorem m and n must be coprime
-- or more specifically m must be in any case smaller than p and q
-- so p and q should be chosen appropriate for all possible messages)

-- decrypts the character m from c with a private key (n, d)
rsaDecryptChar :: (Integer, Integer) -> Integer -> Char
rsaDecryptChar (n, d) c = (chr (fromIntegral (rsaDecrypt (n, d) c)))

-- encrypts the character m to c with a public key (n, e)
rsaEncryptChar :: (Integer, Integer) -> Char -> Integer
rsaEncryptChar (n, e) m = rsaEncrypt (n, e) (toInteger (ord m))
