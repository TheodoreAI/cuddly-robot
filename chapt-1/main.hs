import Data.Char
import System.IO (writeFile, readFile)
{- Syntax for a function for doing more things 
such as these ones to do the cesar cipher -}


{- Syntax for adding values of type Int -}

add :: Int -> Int -> Int
add x y = x + y


{- Syntax for concatenating first and last name -}
fullname :: String -> String -> String
fullname firstName lastName = firstName ++ " " ++ lastName


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


writeEncoded :: Int -> String -> IO ()
writeEncoded shiftValue str = writeFile "output.txt" (encode shiftValue str)


readEncoded :: Int -> IO String
readEncoded shiftValue = do
    contents <- readFile "output.txt" 
    return (encode shiftValue contents)
