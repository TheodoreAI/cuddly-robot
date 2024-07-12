
{-recursion patterns-}

data IntList = Empty | Cons Int IntList
  deriving Show




--Map
absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons(abs x)(absAll xs)
 
