{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}

import Data.List hiding (delete, union)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import System.IO
import Data.Ord
import Data.Int
import Data.Maybe (fromJust)
import Data.Function
import Control.Monad
import Data.Array.IArray
import Data.IORef
import Data.Bits

import System.Exit

import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

import Debug.Trace

data BitSet = BS {
    set  :: {-# UNPACK #-} !Int,
    size :: {-# UNPACK #-} !Int
} deriving (Read, Show)

default (Int)

newBitSet :: Int -> BitSet
newBitSet set = BS set size
    where
        size = length [i | i <- [0..9]
                         , 1 `shiftL` i == (1 `shiftL` i) .&. set] 
    
modifySet (BS set size) foo = BS (foo set) size

zero :: BitSet
zero = newBitSet 0

sub :: BitSet -> BitSet -> BitSet
sub first second = modifySet first (`xor` set first .&. set second)

union :: BitSet -> BitSet -> BitSet
union first second = modifySet first (.|. set second)

isSubsetOf :: BitSet -> BitSet -> Bool
isSubsetOf first second = set first == set first .&. set second

subsets :: BitSet -> [BitSet]
subsets (BS s _ ) = [newBitSet i | i <- [0.. 1 `shiftL` 10]
                                 , i == (i .&. s)]

items :: BitSet -> [BitSet]
items (BS s _ ) = [newBitSet (1 `shiftL` i) | i<- [0..9]
                  , 1 `shiftL` i == (1 `shiftL` i) .&. s
                  ]

instance Eq BitSet where
    a == b = set a == set b

readInts :: ByteString -> [Int]
readInts str | C.null str = []
readInts str = number : readInts (C.tail rest)
    where
        Just (number, rest) = C.readInt str

filename = "cycles"

main = do
    input <- openFile (filename ++ ".in") ReadMode
    [n, m] <- readInts <$> C.hGetLine input
    weight <- readInts <$> C.hGetLine input
    
    list <- forM [1..m]  $ \_ -> do
        (_:nums) <- readInts <$> C.hGetLine input
        return $! newBitSet $! foldl' (\acc n -> acc .|. (1 `shiftL` (n-1))) 0 nums
    let !indep = IS.fromList (set <$> list)
        !indexedWeights = sortBy (comparing (Down . fst)) $ zip weight [0..] :: [(Int, Int)]

    writeFile (filename ++ ".out") $ show $ sum $ snd $ 
        mapAccumL (\acc (w::(Int, Int)) -> let new = union acc (newBitSet $ 1 `shiftL` snd w) in 
                if not (any1 ((`isSubsetOf` new) . newBitSet)  indep) then 
                    traceShowId (new, fst w)
                else (acc, 0)) zero indexedWeights

any1 :: (Int -> Bool) ->IntSet ->  Bool
any1 predicat set = IS.foldl (\x key -> x || predicat key) False set