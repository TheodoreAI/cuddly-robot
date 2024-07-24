import System.Console.Terminfo (Point)
-- chapt-8: Declaring Types and Classes, and using them 
-- They are useful for defining the types of functions and values, and for defining the types of the functions themselves.


-- type declarations
type Position = (Int, Int)
type Move = (Int, Int)

type Point2D = (Float, Float)
type Vector2D = (Float, Float)

type RGB = (Int, Int, Int)


-- data declarations
data Move' = North | South | East | West deriving Show
data Point2D' = Point2D' Float Float deriving Show
data Vector2D' = Vector2D' Float Float deriving Show
data RGB' = RGB' Int Int Int deriving Show


-- function using the type declarations
move :: Move' -> Position -> Position
move North (x, y) = (x, y+1)
move South (x, y) = (x, y-1)
move East (x, y) = (x+1, y)
move West (x, y) = (x-1, y)


-- function using the data declarations
color :: RGB' -> String
color (RGB' 0 0 0) = "black"
color (RGB' 255 255 255) = "white"
color (RGB' 255 0 0) = "red"
color (RGB' 0 255 0) = "green"
color (RGB' 0 0 255) = "blue"





main:: IO ()
main = do
    print $ move North (0, 0) -- (0, 1)
    print $ move South (0, 0) -- (0, -1)
    print $ move East (0, 0) -- (1, 0)
    print $ move West (0, 0) -- (-1, 0)

    print $ color (RGB' 0 0 0) -- black


