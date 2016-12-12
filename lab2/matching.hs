{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Data.List hiding (delete, union)
import qualified Data.ByteString.Char8 as C
import System.IO
import Data.Ord
import Data.Int
import Data.Maybe (fromJust)
import Data.Function
import Control.Monad
import Control.Monad.ST
import Data.Array.MArray
import Data.Array.IO
import Data.IORef

{-# INLINE readInt #-}
readInt x = fst $! fromJust $! C.readInt x


{-# INLINE modifyArray #-}
modifyArray :: (MArray a e m, Ix i, Monad m) => a i e -> i -> (e -> e) -> m ()
modifyArray arr i foo = do
    x <- readArray arr i 
    let x' = foo x
    x' `seq` writeArray arr i x'

{-# INLINE (<!>) #-}
(<!>) :: (MArray a e m, Ix i) => a i e -> i -> m e
(<!>) = readArray

filename = "matching"

main = do
    input  <- openFile (filename ++ ".in") ReadMode
    output <- openFile (filename ++ ".out") WriteMode
    n <- readInt <$> C.hGetLine input :: IO Int
    graph <- newArray (1, n) []   :: IO (IOArray Int [Int])
    --match <- newArray (0, n-1) (-1) :: 
    (weights :: [(Int, Int)]) <- sortBy (comparing $ Down . snd) . zip [1..] . map read . words <$> hGetLine input
    forM_ [1..n] $! \i -> do
        (_:list) <- map readInt . C.words <$> C.hGetLine input
        modifyArray graph i (list ++)
    match <- newArray (1, n) 0
    forM_ weights $ \(i, _) -> dfs i graph match =<< newArray (1, n) False 
    result <- newArray (1, n) 0 :: IO(IOUArray Int Int)
    forM_ [1..n] $ \i -> do
        mt <- match <!> i
        when (mt > 0) $ do
            writeArray result mt i
    forM_ [1..n] $ \i -> do
        hPutStr output . show =<< result <!> i
        hPutStr output " "
    hClose output

dfs :: Int -> IOArray Int [Int] -> IOUArray Int Int -> IOUArray Int Bool -> IO Bool
dfs i graph match used = do
    u <- used <!> i
    if u then
        return False
    else do
        writeArray used i True
        helper =<< graph <!> i
        
    where
        helper [] = return False
        helper (to:rest) = do
            mt <- match <!> to
            if mt == 0 then
                writeArray match to i >> return True
            else do
                d <- dfs mt graph match used
                if d then
                    writeArray match to i >> return True
                else
                    helper rest