{- List comprehensions comes from the mathematical set comprehension notation {x^2 | x epsilon{1..5}-}

listComprehension :: [Integer]
listComprehension = [x^2 | x <- [1..5]]

-- Takes in n
-- "|" is read 'such as', "<-" is read 'is drawn from'
-- x <- [1..5] is called a 'generator'
makeComp :: Integer -> [Integer]
makeComp n = [x^2 | x <- [1..n]]


{- List comprehensions can also use 'guards' (logical expressions to filter out values) -}
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0] -- uses guard n `mod` x == 0

{- lets figure out if a factor is prime-}
prime :: Int -> Bool
prime n = factors n == [1,n]


{- primes to a certain number -}
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x] -- uses guard prime x

{- -}


