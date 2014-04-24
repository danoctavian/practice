module Algos where

import Prelude as P
import Control.Monad.ST
import Data.Vector.Mutable as DVM
import Data.Vector as DV
import Control.Monad as CM
import Control.Monad.Primitive
{-

implementations for 
  * sorting algos
    ** merge
    ** quick
  * hashtables
  * trees
    ** avl
    ** red-black
    ** bfs
    ** dfs
  * graphs


  IDEA: test them with quick-check against real implementations
  can we do any benchmarking?
-}

{-
TimeComplexity = O(n * log(n))
SpaceComplexity = O(n) additional space
-}
mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort fstHalf) (mergeSort sndHalf)
  where
    (fstHalf, sndHalf) = P.splitAt (P.length xs `div` 2)  xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x : xs) (y : ys) = if' (x < y) (x : merge xs (y : ys)) (y : merge (x : xs) ys)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) = (quickSort smaller) P.++ (x  : (quickSort bigger))
  where
    smaller = P.filter (x > ) xs
    bigger = P.filter (x <= ) xs 

{- this obviously cancels out the space advantage but it's to showcase qSortInPlace
 which hopefully doesn't -}
quickSortInPlace :: (Ord a) => [a] -> [a]
quickSortInPlace list = runST $ do
  v <- DV.thaw $ DV.fromList list
  qSortInPlace v 0 (DVM.length v - 1)
  frozen <- DV.freeze v
  return $ DV.toList frozen
  
-- the real shit (not too sure if it saves memory but it sure looks imperative)
-- picking pivot as the first element
qSortInPlace v start end
  | start >= end = return () -- all sorted
  | otherwise = do
      pivotIndex <- qspartition v start end start -- pivot is the first element
      qSortInPlace v start pivotIndex
      qSortInPlace v (pivotIndex + 1) end

-- asume start -end >= 1 (more than 2 elems)
qspartition :: (PrimMonad m, Ord a ) => MVector (PrimState m) a -> Int -> Int -> Int -> m Int
qspartition v start end pivotIndex = do
  pivot <- DVM.read v pivotIndex
  DVM.swap v pivotIndex end
  storeIndex <- CM.foldM (\storeIndex i -> do {
                  vi <- DVM.read v i;
                  if' (vi <= pivot)
                    (DVM.swap v i storeIndex >> (return $ storeIndex + 1))
                    (return storeIndex)})
                start [start..(end - 1)]
  swap v storeIndex end
  return storeIndex -- where 

-- Trees


-- UTILS
if' b x y = if b then x else y
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c x y = c >>= \z -> if z then x else y

