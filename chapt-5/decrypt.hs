import Data.Char

-- string comprehensions
lowers :: String -> Int
lowers xs = length [x | x <- xs, x >= 'a' && x <= 'z']

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x == x']

-- frequency table for english alphabet
table :: [Float]
table = [8.17, 1.49, 2.78, 4.25, 12.70, 2.23, 2.02, 6.09, 6.97, 0.15, 0.77, 4.03, 2.41, 6.75, 7.51, 1.93, 0.10, 5.99, 6.33, 9.06, 2.76, 0.98, 2.36, 0.15, 1.97, 0.07]

-- percent calculation
percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

--freqs
freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = lowers xs

{-Use the chi-square statistic as a standard method for comparing a list of observed frequencies os with alist of observed expected frequencies es -}

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum[((o-e)^2)/e | (o,e) <- zip os es]

-- rotate the elements of a list n places to the left; wrapping around at start of list, and assume the integer n is between 0 and n

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs -- drop and take are keywords

-- positions at which desired value occurs
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..], x == x']

{- Cesar cipher code: to decode just do a negative shift encode (-n) "jarbasdfas" of the same value -}
let2int :: Char -> Int
let2int cesar = ord cesar - ord 'a'

int2let :: Int -> Char
int2let n = chr (ord 'a' + n)


shift :: Int -> Char -> Char
shift n c
    | isLower c = int2let ((let2int c + n) `mod` 26)
    | otherwise = c


encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

-- cracking the cesar cipher using freq table
crack :: String -> String
crack xs = encode(-factor) xs
  where
     factor = head (positions (minimum chitab) chitab)
     chitab = [chisqr (rotate n table') table | n <- [0..25]]
     table' = freqs xs

  
