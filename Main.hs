-- simple demonstration of the RSA cryptography

module Main where

import RSA

main = do
    -- choose two distinct primes p and q
    let p = 257
    let q = 263

    -- generate a key pair from p and q
    let keyPair = rsaGenerateKeyPair p q

    -- retrieve the private and public key from the key pair
    let privateKey = rsaPrivateKey keyPair
    let publickey = rsaPublicKey keyPair

    -- show the private and public key
    let n = rsaModulus privateKey
    let d = rsaExponent privateKey
    let e = rsaExponent publickey

    putStrLn ("private key: " ++ show (n, d))
    putStrLn ("public key:  " ++ show (n, e))

    putStrLn ("")

    -- create a decrypt function by passing the private key to rsaDecryptChar
    let decrypt = rsaDecryptChar privateKey

    -- create a encrypt function by passing the public key to rsaEncryptChar
    let encrypt = rsaEncryptChar publickey

    -- create a message
    let m = "RSA is an asymmetric cryptographic method."

    -- show the message
    putStrLn ("message:   " ++ show m)

    -- encrypt the message
    let em = map encrypt m

    -- show the encrypted message
    putStrLn ("encrypted: " ++ show em)

    -- decrypt the encrypted message
    let dm = map decrypt em

    -- show the decrypted message
    putStrLn ("decrypted: " ++ show dm)

    -- output:
    -- private key: (67591,22357)
    -- public key:  (67591,67069)
    --
    -- message:   "RSA is an asymmetric cryptographic method."
    -- encrypted: [4631,18389,8550,13880,52738,12210,13880,29886,685,13880,29886,12210,31898,557,557,23494,9351,1226,52738,28384,13880,28384,1226,31898,55303,9351,49461,65660,1226,29886,55303,27170,52738,28384,13880,557,23494,9351,27170,49461,28962,4906]
    -- decrypted: "RSA is an asymmetric cryptographic method."
