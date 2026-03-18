{-# LANGUAGE BangPatterns #-}
--  helper for 1st task
-- isPrime :: Int -> Bool
-- isPrime n
--     | n <= 1    = False
--     | otherwise = null [ x | x <- [2 .. floor (sqrt (fromIntegral n))], n `mod` x == 0 ]

-- Task 1: Goldbach Pairs
goldbachPairs :: Int -> [(Int, Int)]
goldbachPairs n = [ (p, q) | p <- [2 .. n `div` 2]
                           , let q = n - p
                           , isPrime p
                           , isPrime q ]

-- Task 2: Coprime Pairs
coprimePairs :: [Int] -> [(Int, Int)]
coprimePairs xs = [ (x, y) | x <- xs
                           , y <- xs
                           , x < y
                           , gcd x y == 1 ]

-- Task 3: Sieve of Eratosthenes
sieve :: [Int] -> [Int]
sieve []     = []
sieve (p:xs) = p : sieve [ x | x <- xs, x `mod` p /= 0 ]

primesTo :: Int -> [Int]
primesTo n = sieve [2..n]

isPrime :: Int -> Bool
isPrime n
    | n <= 1    = False
    | otherwise = n `elem` primesTo n


-- Task 4: Matrix multiplication
matMul :: [[Int]] -> [[Int]] -> [[Int]]
matMul a b = 
  [ [ sum [ a !! i !! k * b !! k !! j | k <- [0 .. p - 1] ] | j <- [0 .. n - 1] ] | i <- [0 .. m - 1] ]
  where
    m = length a
    p = length (head a)
    n = length (head b)

-- Task 5: Permutations
permutations :: Int -> [a] -> [[a]]
permutations 0 _  = [[]]
permutations k xs = 
  [ (xs !! i) : rest | i <- [0 .. length xs - 1] 
                     , let leftovers = take i xs ++ drop (i + 1) xs
                     , rest <- permutations (k - 1) leftovers ]


-- Task 6a: Merging two sorted lists without duplicates
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x < y     = x : merge xs (y:ys)
  | x > y     = y : merge (x:xs) ys
  | otherwise = x : merge xs ys

-- Task 6b: The infinite list of Hamming numbers
hamming :: [Integer]
hamming = 1 : merge (map (*2) hamming) (merge (map (*3) hamming) (map (*5) hamming))

-- Task 7: Integer Power with Bang Patterns 
power :: Int -> Int -> Int
power base exponent = go base exponent 1
  where
    go _ 0 !acc = acc
    go b e !acc = go b (e - 1) (acc * b)

-- Task 8, seq version
listMaxSeq :: [Int] -> Int
listMaxSeq [] = error "Empty list"
listMaxSeq (x:xs) = go xs x
  where
    go [] acc = acc
    go (y:ys) acc = let newMax = max y acc 
                    in newMax `seq` go ys newMax

-- Bang patterns 
listMaxBang :: [Int] -> Int
listMaxBang [] = error "Empty list"
listMaxBang (x:xs) = go xs x
  where
    go [] !acc = acc
    go (y:ys) !acc = go ys (max y acc)

-- Task 9: inf prime stream 
primes :: [Int]
primes = sieve [2..]

-- b) part 
isPrimeInf :: Int -> Bool
isPrimeInf n
    | n <= 1    = False
    | otherwise = n `elem` takeWhile (<= n) primes

-- task 10: Strict accumulations and space leaks 
-- no strictness annotations
meanLazy :: [Double] -> Double
meanLazy xs = let (totalSum, totalLen) = go xs 0 0 in totalSum / totalLen
  where
    go [] s l = (s, l)
    go (y:ys) s l = go ys (s + y) (l + 1)

-- fixed space leak with bang patterns
meanStrict :: [Double] -> Double
meanStrict xs = let (totalSum, totalLen) = go xs 0 0 in totalSum / totalLen
  where
    go [] !s !l = (s, l)
    go (y:ys) !s !l = go ys (s + y) (l + 1)

-- Mean and variance in one pass
meanAndVar :: [Double] -> (Double, Double)
meanAndVar xs = 
    let (sumX, sumSq, count) = go xs 0 0 0
        mu = sumX / count
        var = (sumSq / count) - (mu * mu)
    in (mu, var)
  where
    go [] !s !sq !n = (s, sq, n)
    go (y:ys) !s !sq !n = go ys (s + y) (sq + y * y) (n + 1)

main :: IO ()
main = do
    print (goldbachPairs 4)
    print (goldbachPairs 28)
    print (goldbachPairs 100)
    print (coprimePairs [2, 4, 3, 6, 5])
    print (isPrime 2)
    print (isPrime 13)
    print (isPrime 15)
    print (primesTo 20)
    let a = [[1, 2, 3], [4, 5, 6]]
    let b = [[7, 8], [9, 10], [11, 12]]
    print (matMul a b)
    print (permutations 2 [1, 2, 3])
    print (permutations 3 "abc")
    print (merge [1, 3, 5] [2, 3, 4])
    print (take 20 hamming)
    print (listMaxSeq [3, 1, 4, 1, 5, 9])
    print (listMaxBang [3, 12, 4, 1, 5, 9])
    print (take 20 primes)
    print (isPrimeInf 29)
    print (isPrimeInf 30)
    print (meanLazy [1.0, 2.0, 3.0, 4.0])
    print (meanStrict [1.0, 2.0, 3.0, 4.0])
    print (meanAndVar [1.0, 2.0, 3.0, 4.0])
    print(power 2 10)
    print(power 3 5)

    