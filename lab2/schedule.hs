{-# OPTIONS_GHC -O2 #-}

import Data.List hiding (delete)
import System.IO
import           Data.Set (Set, deleteMax, delete, lookupLE)
import qualified Data.Set as S
import Data.Ord
import Data.Int
import Data.Maybe (fromJust)
import Control.Monad

filename = "schedule"

parseInts input = do
    [x, y] <- map read . words <$> hGetLine input
    return (x, y)

main = do
    input <- openFile (filename ++ ".in") ReadMode
    n <- read <$> hGetLine input
    list <- forM [1..n] $! \_ -> parseInts input
    writeFile (filename ++ ".out") $! show $! solve (sortBy (comparing $ Down . snd) list) n

solve :: [(Int, Int)] -> Int -> Int64
solve list n = helper (S.fromList [1..n]) 0 list
    where
        helper :: Set Int -> Int64 -> [(Int, Int)] -> Int64
        helper set acc []                  = acc
        helper set acc ((time, fine):rest) = 
            case lookupLE time set of
                Nothing  -> helper (deleteMax set) (acc + fromIntegral fine) rest
                Just tmp -> helper (delete tmp set) acc rest 

