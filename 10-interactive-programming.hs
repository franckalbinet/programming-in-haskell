-- 10.4 Sequencing
act :: IO (Char,Char)
act = do x <- getChar
         getChar
         y <- getChar
         return (x,y)

-- 10.5 Derived primitives
getLine' :: IO String
getLine' = do x <- getChar
              if x == '\n' then
                           return []
              else 
                  do xs <- getLine'
                     return (x:xs)

putStr' :: String -> IO ()
putStr' []     = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStrLn' :: String -> IO ()
putStrLn' xs = do putStr' xs
                  putChar '\n'

strlen :: IO ()
strlen = do putStr "Enter a string: "
            xs <- getLine
            putStr "The string has "
            putStr (show (length xs))
            putStrLn " characters"

-- 10.6 Hangman
hangman :: IO ()
hangman = do putStrLn "Think of a word:"
             word <- sgetline
             putStrLn "Try to guess it:"
             play word

sgetLine :: IO String
sgetLine = do x <- getCh
              if x == '\n` then
                 do putChar x
                    return []
              else
                 do putChar '-'
                    xs <-
               
