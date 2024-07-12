{-enumeration types-}

data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

shoe :: Thing
shoe = Shoe

listOfThings :: [Thing]
listOfThings = [Shoe, SealingWax, King, Cabbage, King]


isSmall :: Thing -> Bool
isSmall Shoe = True

{-Data constructors with more than one argument-}
-- Store a person's name, age, and favorite Thing
data Person = Person String Int Thing 
  deriving Show

paloma :: Person
paloma = Person "Paloma Estrada" 26 Shoe

mateo :: Person
mateo = Person "Mateo Estrada" 27 Cabbage


getAge :: Person -> Int
getAge (Person _ a _) = a

getName :: Person -> String
getName (Person b _ _) = b


-- Recursive data types can be used for trees: think BFS/DFS and sorting abstract syntax trees

data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

 

