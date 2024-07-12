{-it is a function that takes an integer and returns and integer -}

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial(n-1)

{-hailstone-}

hailstone :: Integer -> Integer
hailstone n
 | n `mod` 2 == 0 = n `div` 2
 | otherwise      = 3*n + 1

hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

{-checking if integer is even-}

isEven :: Integer -> Bool
isEven n
 | n `mod` 2 == 0 = True
 | otherwise      = False

{-Pairs-}

p :: (Int, Char)
p = (3, 'x') 

sumPair :: (Int, Int) -> Int
sumPair (x, y) = x + y


{-lists-}
sumList :: Int -> Int
sumList x = sum[1..x]

listOfLength :: Int -> [Int]
listOfLength n = [0..n]

{-cons operator-}

constList = [2, 3, 4] == 2 : 3 : 4: []

{-writing functions on lists using pattern matching-}
intListLength :: [Integer] -> Integer
intListLength [] = 0
intListLength (x:xs) = 1 + intListLength xs


{-nested patterns-}
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo [] = []
sumEveryTwo (x:[]) = [x]
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs


{-lets combine functions now-}

hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1





