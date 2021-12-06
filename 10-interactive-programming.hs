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
            
