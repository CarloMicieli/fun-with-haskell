{- #############################################################################
Sample code from:
Simon Thompson - Haskell: the Craft of Functional Programming, 2011
++++ Addison-Wesley ++++
http://www.haskellcraft.com/craft3e/Home.html
############################################################################# -}

helloWorld :: IO ()
helloWorld = putStrLn "Hello world"

printLn :: (Show a) => a -> IO ()
printLn = putStrLn . show

print2 :: String -> IO ()
print2 str = do putStrLn str
                putStrLn str

print4ln :: String -> IO ()
print4ln str = do putStrLn str
                  putStrLn str
                  putStrLn str
                  putStrLn str

read2lines :: IO String
read2lines = do l1 <- getLine
                l2 <- getLine
                let str = l2 ++ " " ++ l1
                return str

{- Ex 8.10 -}
checkPalin :: IO Bool
checkPalin = do l <- getLine
                return $ isPalin l
    where isPalin s = s == reverse s

{- Ex 8.11 -}
sumNums :: IO Int
sumNums = do a <- getLine
             b <- getLine
             let n1 = read a :: Int
             let n2 = read b :: Int
             return (n1 + n2)

{- Ex 8.12 -}
putNtimes :: Integer -> String -> IO ()
putNtimes n str = loop putStrLn n str

loop :: (a -> IO ()) -> Integer -> a -> IO ()
loop f n s = do if n == 0
                then return ()
                else do
                  (f s)
                  loop f (n-1) s

{- Ex 8.13 -}
getInt :: IO Integer
getInt = do l <- getLine
            let n = read l :: Integer
            return n

sums :: Integer -> Integer -> IO Integer
sums 0 acc = return acc
sums n acc = do num <- getInt
                sums (n - 1) (acc + num)

sumNints :: IO ()
sumNints = do putStr "Please, enter how many numbers: "
              n <- getInt
              res <- sums n 0
              putStrLn $ "Sum = " ++ show res

copy :: IO ()
copy = do line <- getLine
          putStrLn line
          copy

copyN :: Integer -> IO ()
copyN 0 = return ()
copyN n = do line <- getLine
             putStrLn line
             copyN (n - 1)

copyEmpty :: IO ()
copyEmpty = do line <- getLine
               if null line
               then return ()
               else do putStrLn line
                       copyEmpty

copyCount :: Integer -> IO ()
copyCount n = do line <- getLine
                 if null line
                 then putStrLn $ "read " ++ show n ++ " line(s)"
                 else copyCount (n + 1)
