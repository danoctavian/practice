{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module HashTable where

import Prelude as P
import Control.Monad.ST
import Data.Vector.Mutable as DVM
import Data.Vector as DV
import Control.Monad as CM
import Control.Monad.Primitive
import Data.Maybe
import Data.Hashable
import Data.List as DL
import Data.STRef
import Control.Monad.ST.Class
import Data.Vector.Instances

import Utils

-- try immutable hashtable... probably not efficient

--  data HashTable = HashTable 
data MHashTable s k v = HTRef {tableRef :: STRef s (HashTable s k v)}
data HashTable s k v  = HashTable {size :: Int, elemCount :: Int, array :: (MVector s [(k, v)])}

defaultSize = 10
resizeRatio = 0.75 -- how filled should it be when you decide to resize


new = newOfSize defaultSize
newOfSize size = DVM.replicate size [] >>= (liftST . newSTRef . (HashTable defaultSize 0)) >>= (return . HTRef)


hashKey :: (Hashable k) => HashTable s k v -> k -> Int
hashKey ht k = hash k `mod` (size ht)

readRef ht = liftST $ readSTRef $ tableRef ht
get htRef k = readRef htRef >>= \ht -> DVM.read (array ht) (hashKey ht k) >>= (return . (DL.lookup k))

remove htRef k = readRef htRef >>= \ht -> let h = hashKey ht k in 
                  DVM.read (array ht) h >>= (DVM.write (array ht) h) . (P.filter ( (k /=) . fst))
                  >> writeSTRef (tableRef htRef) (ht {elemCount = elemCount ht - 1})


htSize htRef = readRef htRef >>= return . size
htSizeElemCount htRef = readRef htRef >>= return . elemCount

put htRef k v = readRef htRef >>= \ht -> insertKV ht k v >> if' (tooBig ht)
                (resize ht (resizeSize ht)) (return $ ht {elemCount = elemCount ht + 1})
                >>= writeSTRef (tableRef htRef)

resize oldHT newSize = do
  oldList <- mHTToList oldHT
  newHT <- fmap (HashTable newSize (P.length oldList)) $ DVM.replicate newSize []
  CM.forM oldList (\(k, v) -> insertKV newHT k v)
  return newHT 
  

-- modify the array by adding an element
-- ignoring dups atm
insertKV ht k v = let h = hashKey ht k in DVM.read (array ht) h
                      >>= (DVM.write (array ht) h)  . ((k, v) :)

mHTToList ht = fmap (DL.concat . DV.toList) $ freeze (array ht)

tooBig (HashTable size elems _) = (fromIntegral elems) >= resizeRatio * (fromIntegral size)

resizeSize (HashTable size _ _) = 2 * size


{-
runHT = do
  ht <- HashTable.new
  let range = [1..100]
  CM.forM range $ \x -> put ht (show x) x 
  CM.forM range $ \x -> get ht (show x) 
  -}
  
