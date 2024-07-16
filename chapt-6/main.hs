-- Basic concepts of recursion in haskell

fac :: Int -> Int -- factorial function
fac n = product [1..n]

-- redefining it using recursion
factorial :: Int -> Int 
factorial 0 = 1 -- base case
factorial n = n * factorial (n-1) -- recursive case


{- Defining functions using recursion allows properties of those functions to be proved using the simple
but powerful technique of induction -}

-- example using the (*) operator
-- (*) :: Int -> Int -> Int
-- m * 0 = 0
-- m * n = m + (m * (n-1))


-- Recursion with lists

prod :: Num a => [a] -> a
prod [] = 1 -- base case
prod (n:ns) = n * prod ns -- recursive case


-- lets define python's len
len :: [a] -> Int
len [] = 0 -- base case
len (_:xs) = 1 + len xs -- recursive case (we use the _ to ignore the values and thus we can pass Int, Char, etc)

-- recursive definition of reversing a list
rev :: [a] -> [a]
rev [] = [] -- base case
rev (x:xs) = rev xs ++ [x] -- recursive case

-- defining my own append operator (use it as [listOne] `append` [listTwo])
append :: [a] -> [a] -> [a]
[] `append` ys = ys -- base case
(x:xs) `append` ys = x : (xs `append` ys) -- recursive case


-- insertion sort algorithm in two functions
insert :: Ord a => a -> [a] -> [a] -- a has a typeclass Ord
insert x [] = [x] -- base case
insert x (y:ys) | x <=  y = x : y : ys -- recursive
                | otherwise = y : insert x ys -- case


insertionSort :: Ord a => [a] -> [a]
insertionSort [] = [] -- base case
insertionSort (x:xs) = insert x (insertionSort xs) -- recursive case


main :: IO ()
main = do 
    print $ insertionSort [3, 2, 1, 4, 5] -- [1, 2, 3, 4, 5]

