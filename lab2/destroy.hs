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
import Data.Array.ST
import Data.IORef

--import Debug.Trace

data DSU s = DSU {
    parent :: STUArray s Int Int,
    rank   :: STUArray s Int Int
}

root :: DSU s -> Int -> ST s Int
root dsu i = do
    p <- parent dsu <!> i
    if (p == i) then
        return i
    else do
        r <- root dsu p
        writeArray (parent dsu) i r
        return r

connected :: DSU s -> Int -> Int -> ST s Bool
connected dsu = (liftM2 (==)) `on` (root dsu)

union :: DSU s -> Int -> Int -> ST s ()
union dsu i j = do
    a <- root dsu i
    b <- root dsu j
    when (a /= b) $ do
        rankA <- rank dsu <!> a
        rankB <- rank dsu <!> b
        let ((minw, mini), (maxw, maxi)) = minMax (rankA, a) (rankB, b)
        writeArray (parent dsu) mini maxi 
        writeArray (rank dsu) maxi (minw + maxw)

newDSU :: Int -> ST s (DSU s)
newDSU size = do
    p <- newListArray (1, size) [1..size]
    r <- newArray (1, size) 0
    return $! (DSU p r)

minMax :: (Ord a) => a -> a -> (a, a)
minMax x y | x > y     = (x, y)
           | otherwise = (y, x)

{-# INLINE modifyArray #-}
modifyArray :: (MArray a e m, Ix i, Monad m) => a i e -> i -> (e -> e) -> m ()
modifyArray arr i foo = do
    x <- readArray arr i 
    let x' = foo x
    x' `seq` writeArray arr i x'

{-# INLINE (<!>) #-}
(<!>) :: (MArray a e m, Ix i) => a i e -> i -> m e
(<!>) = readArray

{-# INLINE parseInts #-}
parseInts input = do
    line <- hGetLine input
    let [x, y, z] = words line
    return $! (read $! x, read $! y, read $! z)

filename = "destroy"

main = do
    input  <- openFile (filename ++ ".in") ReadMode
    output <- openFile (filename ++ ".out") WriteMode
    (n :: Int, m :: Int, s_max :: Int64) <- parseInts input
    list <- forM [1..m] $ \i -> do
        (x, y, z) <- parseInts input
        return (i, x, y, z)
    let !list1 = sortBy (comparing $ \(_, _, _, s) -> Down s) list
    flammable <- reverse <$!> stToIO (spanningTree list1 n)
    let (i, indexes) = solve flammable s_max
    hPrint output i
    mapM_ (hPutShowSpace output) (sort indexes)
    hClose output

hPutShowSpace output x = hPutStr output (show x) >> hPutStr output " "

type Graph = [(Int, Int, Int, Int64)]

spanningTree :: Graph -> Int -> ST s Graph
spanningTree graph n = newDSU n >>= \d -> helper graph d []
    where
        helper :: Graph -> DSU s -> Graph -> ST s Graph
        helper [] dsu !acc = return $! acc
        helper (a@(i, from, to, size):rest) dsu !acc = do
            c <- connected dsu from to
            if c then
                helper rest dsu (a:acc)
            else do
                union dsu from to
                helper rest dsu acc

solve :: Graph -> Int64 -> (Int, [Int])
solve deletable maxSize = burn deletable maxSize 0 []
    where
        burn :: Graph -> Int64 -> Int -> [Int] -> (Int, [Int])
        burn [] !space !num !acc = (num, acc)
        burn ((i, from, to, size):rest) !space !num !acc
            | size > space = burn rest space num acc
            | otherwise    = burn rest (space - size) (num + 1) (i:acc)