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


{- crumb list has an associated length...
- retarded but classical lists don't allow for length in O(1) :(
-}

type Crumbs a = ([Crumb a], Int)
type GoAction a = (TreeZipper a) -> Maybe (TreeZipper a)
data Crumb a = LeftCrumb { crumbVal:: a, crumbTree :: (BinTree a)}
            | RightCrumb { crumbVal:: a, crumbTree :: (BinTree a)}
  deriving (Show)

instance (Eq a) => Eq (Crumb a) where
  c1 == c2 = crumbVal c1 == crumbVal c2

type TreeZipper a = (BinTree a, Crumbs a)
data Direction = L | R
  deriving (Show, Eq)

toDir (LeftCrumb _ _) = L
toDir (RightCrumb _ _) = R

-- the dir taken to get to the current node
direction :: TreeZipper a -> Maybe Direction
direction (_, ([],0)) = Nothing
direction (_, (c : cs, _)) = Just $ toDir c

goLeft :: GoAction a
goLeft (Leaf _, _) = Nothing
goLeft (Node v left right, cs) = Just (left, addCrumb (LeftCrumb v right) cs) 

goRight :: GoAction a
goRight (Leaf _, _) = Nothing
goRight (Node v left right, cs) = Just (right, addCrumb (RightCrumb v left) cs) 


goUp :: GoAction a
goUp (_, ([], 0)) = Nothing 
goUp (t, ((LeftCrumb v r ) : bs, d) ) = Just (Node v t r, (bs, d - 1))
goUp (t, ((RightCrumb v l) : bs, d) ) = Just (Node v l t, (bs, d- 1))

addCrumb :: Crumb a -> Crumbs a ->  Crumbs a
addCrumb c (cs, depth) = (c : cs, depth + 1)

goBack :: (Eq a) => (TreeZipper a) -> GoAction a
goBack orig@(origin, origBs) here@(curr, currBs) 
  = P.foldl (>>=) (return here) $ (P.replicate (P.length pathUp) goUp)
                                  P.++ (if' movesUpDown [goBrother] [])
                                  P.++ (P.map toMove pathDown)
    where
      toMove (LeftCrumb _ _) = goLeft
      toMove (RightCrumb _ _) = goRight
      movesUpDown = (pathUp /= [] && pathDown /= [])
                    || (val .fst $ orig) /= (val .fst $ here)
      (pathUp, pathDown) = diff currBs origBs 

diff :: (Eq a) => ([a], Int) -> ([a], Int) -> ([a], [a])
diff (xs, lenx) (ys, leny) = (startXs P.++ diffX, startYs P.++ diffY)
  where
    d = lenx - leny 
    (startXs, restXs) = P.splitAt d xs
    (startYs, restYs) = P.splitAt (-d) ys
    (diffX, diffY) =P.unzip $ P.takeWhile (\(x, y) -> x /= y) $ P.zip restXs restYs



goGrandParent :: GoAction a
goGrandParent t = goUp t >>= goUp

goBrother :: GoAction a 
goBrother tz = do
  dir <- direction tz
  parent <- goUp tz
  if' (dir == L) goRight goLeft $ parent

goUncle :: GoAction a
goUncle tz = goUp tz >>= goBrother


hitAndRun :: (Eq a) => (a -> a) -> GoAction a -> GoAction a
hitAndRun f goThere here = return here >>= goThere >>= (return . (modifyV f)) >>= (goBack here)

goTop :: TreeZipper a -> TreeZipper a
goTop (t, ([], 0)) = (t, ([], 0))
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

-- red black tree- finally
data Color = Black | Red
  deriving (Show, Eq, Ord)
type RBElem a = Maybe (a, Color)
type RBTree a = (Ord a) => BinTree (RBElem a)

type RBZipper a = TreeZipper (RBElem a)

setColor c = fmap $ \(x, _) -> (x, c) 

insertRB :: (Ord a) => a -> TreeZipper (RBElem a) -> TreeZipper (RBElem a)
insertRB x tz
  = goTop $ balance $ insertOrd (Just (x, Red)) tz 

balance :: (Ord a) => TreeZipper (RBElem a) -> TreeZipper (RBElem a) 
balance node
  | isNothing $ goUp node = modifyV (setColor Black) node
  | (== Black) . snd . fromJust . val . tree . fromJust . goUp $ node = node -- parent is black

  | toBool (goUncle node >>= (val . tree) >>= (return . (== Red) . snd ) )
      = balance . fromJust $ (hitAndRun (setColor Black) goUp node)
        >>= (hitAndRun (setColor Black) goUncle)
        >>= goGrandParent >>= (Just . (modifyV (setColor Red)))
  | otherwise = node

rotate :: RBZipper a -> Maybe (RBZipper a)
rotate node = do
  parentDir <- (goUp node >>= direction)
  gParentDir <- (goUp node >>= goUp >>= direction)
  rotated <- case (parentDir, gParentDir) of
    (R, L) -> rotateLeft node >>= goLeft
    (L, R) -> rotateRight node >>= goRight
    _ -> return node
  return rotated
-- the mysterious case 5 ...
case5 :: (Eq a) => RBZipper a -> Maybe (RBZipper a)
case5 node = hitAndRun (setColor Black) goUp node
          >>= (hitAndRun (setColor Red) goGrandParent)
          >>= goGrandParent
          >>= (if' ((fromJust $ goUp node >>= direction) ==L)
                  rotateRight  rotateLeft)

binTree1 = Node "A" (Leaf "1") (Node "B" (Leaf "2") (Leaf "3"))
binTree2 = Node "B" (Node "A" (Leaf "1") (Leaf "2"))  (Leaf "3")

root2 = (binTree2, ([], 0 :: Int))
test1 = goBack (fromJust $ return root2 >>= goLeft) (fromJust $ return root2 >>= goRight)
test2 = return root2 >>= goLeft >>= goLeft >>= hitAndRun (\_ -> "wtf") goUncle 

-- typical bin search three insert with 1 quirk:
-- leaf cloner (it replaces a leaf and appends it and a clone to it)
insertOrd :: (Ord a) => a -> TreeZipper a -> TreeZipper a
insertOrd x tz
  = case (if' (x > (val $ tree tz)) goLeft goRight $ tz) of
    Just down -> insertOrd x down
    Nothing -> modifyT (\old -> Node x old old) tz 


-- UTILS
if' b x y = if b then x else y
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM c x y = c >>= \z -> if z then x else y

toBool Nothing = False
toBool (Just b) = b
