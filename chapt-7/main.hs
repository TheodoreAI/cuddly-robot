-- Chapt 7 is on the idea of higher order functions, which are functions 
-- that take other functions as arguments or return functions as results.

-- 7.1 Curried functions - functions that take their arguments one at a time using the currying technique

suma :: Int -> Int -> Int
suma = \x -> (\y -> x + y) -- only as an example, we can use the normal way


-- 7.2 Higher order functions - functions that take other functions as arguments or return functions as results
dosVeces :: (a -> a ) -> a -> a -- this function takes a function and a value and applies the function twice to the value
dosVeces f x = f (f x) -- this is a higher order function

-- lets do one that applies the function 3 times
tresVeces :: (a -> a) -> a -> a
tresVeces f x = f (f (f x))

-- lets do one that applies the function n times
nVeces :: Int -> (a -> a) -> a -> a
nVeces 0 _ x = x
nVeces n f x = f (nVeces (n-1) f x)
-- use it as nVeces 3 f x, where f is the function and x is the value

-- 7.3 processing lists
-- map applies a function to each element of a list
mapa :: (a -> b) -> [a] -> [b]
mapa f xs = [f x | x <- xs] -- this is the list comprehension way

mapas :: (a -> b) -> [a] -> [b]
mapas _ [] = [] -- base case
mapas f (x:xs) = f x : mapas f xs -- recursive case

-- filter selects the elements of a list that satisfy a predicate
filtro :: (a -> Bool) -> [a] -> [a]
filtro p xs = [x | x <- xs, p x] -- list comprehension way

filtros :: (a -> Bool) -> [a] -> [a]
filtros _ [] = [] -- base case
filtros p (x:xs) | p x = x : filtros p xs -- recursive case
                 | otherwise = filtros p xs -- recursive case

-- prime
prime :: Int -> Bool
prime n = n > 1 && and [n `mod` x /= 0 | x <- [2..n-1]]
                
