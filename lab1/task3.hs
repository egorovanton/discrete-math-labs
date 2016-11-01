module Main where

import System.IO
import System.Exit
import Data.IORef
import Data.Array
import Control.Monad

ask x y = do
    putStrLn $ "1 " ++ (show x) ++ " " ++ (show y)
    answer <- getLine
    return $ answer == "YES"

main = do
    hSetBuffering stdout LineBuffering    
    lampsAmount  <- read <$> getLine :: IO Int 
    when (lampsAmount ==  1) $ do
        putStrLn "0 1"
        exitWith ExitSuccess
    path   <- newIORef [1] :: IO (IORef [Int])
    forM_ [2..lampsAmount] $ \i -> do
        p <- readIORef path
        b <- binSearch 0 (i-1) array $ listArray (0, i-2) $ map (`ask` i) p
        modifyIORef path (insert b i)
    p <- readIORef path
    putStr "0 "
    forM_ p $ \i -> do
        putStr $ show i
        putStr " "

binSearch :: Int -> Int -> Array Int (IO Bool) -> IO Int
binSearch start finish array =
    if finish == start then do
        return start
    else do
        let middle = (start + finish) `div` 2
        m <- array ! middle
        if m then
            binSearch (middle+1) finish array
        else
            binSearch start middle array

insert i x list = before ++ x:after
    where (before, after) = splitAt i list  