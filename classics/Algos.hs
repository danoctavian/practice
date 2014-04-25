{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
module Algos where

import Prelude as P
import Control.Monad.ST
import Data.Vector.Mutable as DVM
import Data.Vector as DV
import Control.Monad as CM
import Control.Monad.Primitive
import Data.Maybe
-- not using this; gonna do zipping by hand for now
--import Data.Generics.Zipper
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
data NTree a = NTree a [NTree a]
preOrder (NTree x children) = x : (P.concatMap preOrder children)
postOrder (NTree x children) = (P.concatMap preOrder children) P.++ [x]
bfs :: NTree a -> [a]
bfs t = bfsQ [t]
  where 
    bfsQ [] = []
    bfsQ ts = (\(xs, childs) -> xs P.++  (bfsQ $ P.concat childs))
              . P.unzip . (P.map (\(NTree x ch) -> (x, ch))) $ ts
dfs = preOrder
-- example ntrees
ntree1 = NTree 1 [NTree 2 [NTree 5 [], NTree 6 []], NTree 3 [NTree 7 [], NTree 8 []]]


-- red-black tree (shiiit)

-- general bin-tree stuff
data BinTree a = Node { val :: a, leftT :: (BinTree a), rightT :: (BinTree a) } | Leaf { val :: a}
  deriving Show
data Crumb a = LeftCrumb a (BinTree a) | RightCrumb a (BinTree a)
  deriving Show
type TreeZipper a = (BinTree a, [Crumb a])

data Color = Red | Black
type RBElem a = Maybe (a, Color)
type RBTree a = (Ord a) => BinTree (RBElem a)

goLeft :: (TreeZipper a) -> Maybe (TreeZipper a)
goLeft (Leaf _, _) = Nothing
goLeft (Node v left right, cs) = Just (left, (LeftCrumb v right) : cs) 

goRight :: (TreeZipper a) -> Maybe (TreeZipper a)
goRight (Leaf _, _) = Nothing
goRight (Node v left right, cs) = Just (right, (RightCrumb v left) : cs) 

goUp :: (TreeZipper a) -> Maybe (TreeZipper a)
goUp (_, []) = Nothing 
goUp (t, (LeftCrumb v r) : bs) = Just (Node v t r, bs)
goUp (t, (RightCrumb v l) : bs) = Just (Node v l t, bs)

goTop :: TreeZipper a -> TreeZipper a
goTop (t, []) = (t, [])
goTop  tz =  goTop $ fromJust $ goUp tz

modifyT :: (BinTree a -> BinTree a) -> (TreeZipper a) -> TreeZipper a
modifyT f (t, bs) = (f t, bs)

modifyV  :: (a -> a) -> (TreeZipper a) -> TreeZipper a
modifyV f = modifyT (\t -> t {val = f $ val t})

tree = P.fst

rotateLeft :: TreeZipper a -> Maybe (TreeZipper a)
rotateLeft n@(Node v l r, bs)
  = do 
    repl@(Node replV replL replR, _) <- goUp (modifyT (\_ -> r) n)
    left <- goLeft (modifyV (\_ -> v) repl)
    goUp (modifyT (\n -> Node replV n l) left)

rotateRight :: TreeZipper a -> Maybe (TreeZipper a)
rotateRight n@(Node v l r, bs)
  = do 
    repl@(Node replV replL replR, _) <- goUp (modifyT (\_ -> l) n)
    right <- goRight (modifyV (\_ -> v) repl)
    goUp (modifyT (\n -> Node replV r n) right)


binTree1 = Node "A" (Leaf "1") (Node "B" (Leaf "2") (Leaf "3"))
binTree2 = Node "B" (Node "A" (Leaf "1") (Leaf "2"))  (Leaf "3")

-- with Nothing Leaves 
-- leaf cloner
insertOrd :: (Ord a) => (BinTree a) -> TreeZipper (Maybe a) -> TreeZipper (Maybe a)
insertOrd x tz
  = case (val $ tree tz) of
    Just y -> insertOrd x $ fromJust $ (if' (x > y) goLeft goRight) tz
    Nothing -> modifyT (\_ -> Node x emptyLeaf emptyLeaf) tz 

-- UTILS
if' b x y = if b then x else y
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c x y = c >>= \z -> if z then x else y

