import System.IO (writeFile)
-- A batch program is one where you get an input, do some processing, and then output the result
    
    {- inputs --> [Batch Program] --> outputs -}
                    

-- An interactive program is one where you get an input, do some processing, and then output the result, but you also get some input from the user in between

{-
                Keyboard
                    |
                    V
inputs --> [Interactive Program] --> outputs
                    |
                    V
                  Screen  
-}

-- A GUI program is one where you get an input, do some processing, and then output the result, but you also get some input from the user in between, and the user interacts with the program through a graphical user interface

{-
                Keyboard
                    |
                    V
inputs --> [GUI Program] --> outputs
                    |
                    V
                  Screen
-}


-- A web program is one where you get an input, do some processing, and then output the result, but you also get some input from the user in between, and the user interacts with the program through a web browser

{-
                Keyboard
                    |
                    V
inputs --> [Web Program] --> outputs
                    |
                    V
                  Screen
-}

echo :: IO (String)
echo = do
    putStrLn "What is your first name?"
    firstName <- getLine
    return (firstName)

csvFormatUser :: IO (String)
csvFormatUser = do
    putStrLn "What is your first name?"
    firstName <- getLine
    putStrLn "What is your last name?"
    lastName <- getLine
    putStrLn "What is your age?"
    age <- getLine
    putStrLn "What is your email?"
    email <- getLine
    return (firstName ++ ", " ++ lastName ++ ", " ++ age ++ ", " ++ email ++ "\n")



main :: IO ()
main = do
    name <- csvFormatUser
    writeFile "user.csv" name
    putStrLn "Name written to file"
    putStrLn name
    putStrLn "Done"


    
