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

import Control.Monad.Trans.Writer
import Control.Monad.Trans.Class
import Data.Array.MArray
import Data.Array.IO
import Data.IORef

import Data.Set (Set)
import qualified Data.Set as S 

import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
--import Debug.Trace

type DiffList a = [a] -> [a]



data DSU = DSU {
    parent :: IntMap Int,
    rank   :: IntMap Int
} deriving (Read, Show)

root :: DSU -> Int -> Int
root dsu i | p == i    = i 
           | otherwise = root dsu p
    where
        p = parent dsu IM.! i

connected :: DSU -> Int -> Int -> Bool
connected dsu i j = root dsu i == root dsu j

union :: DSU -> Int -> Int -> DSU
union dsu i j | a == b    = dsu
              | otherwise = DSU newParent newRank
    where
        a = root dsu i
        b = root dsu j
        ((minw, mini), (maxw, maxi)) = (minMax `on` (\x -> (rank dsu IM.! x, x))) a b
        newParent = IM.insert mini maxi (parent dsu)
        newRank   = IM.insert maxi (minw + maxw) (rank dsu)

newDSU :: Int -> DSU
newDSU size = DSU {
    parent = IM.fromDistinctAscList [(x, x) | x <- [1..size]],
    rank   = IM.fromDistinctAscList [(x, 0) | x <- [1..size]]
}

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
    --traceM "Hi"
    (n :: Int, m :: Int, s_max :: Int64) <- parseInts input
    list <- forM [1..m] $ \i -> do
        (x, y, z) <- parseInts input
        return (i, x, y, z)
    let list1 = sortBy (comparing $ \(_, _, _, s) -> Down s) list
        !flammable = reverse $! spanningTree list1 n
        (i, indexes) = solve flammable s_max
    --traceShowM list1
    --traceShowM flammable
    --traceShowM indexes
    hPrint output i
    mapM_ (hPutShowSpace output) (sort indexes)
    hClose output

hPutShowSpace output x = hPutStr output (show x) >> hPutStr output " "

type Graph = [(Int, Int, Int, Int64)]

spanningTree :: Graph -> Int -> Graph
spanningTree graph n = helper graph (newDSU n) []
    where
        helper :: Graph -> DSU -> Graph -> Graph
        helper [] dsu acc = acc
        helper (a@(i, from, to, size):rest) dsu  acc
            | connected dsu from to = helper rest dsu (a:acc)
            | otherwise = helper rest (union dsu from to) acc

solve :: Graph -> Int64 -> (Int, [Int])
solve deletable maxSize = burn deletable maxSize 0 []
    where
        burn :: Graph -> Int64 -> Int -> [Int] -> (Int, [Int])
        burn [] !space !num !acc = (num, acc)
        burn ((i, from, to, size):rest) !space !num !acc
            | size > space = burn rest space num acc
            | otherwise    = burn rest (space - size) (num + 1) (i:acc)