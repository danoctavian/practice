
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module BinTree where

import Prelude as P
import Control.Monad.ST
import Data.Vector.Mutable as DVM
import Data.Vector as DV
import Control.Monad as CM
import Control.Monad.Primitive
import Data.Maybe
import Data.List as DL

import Utils

-- general bin-tree stuff
-- with zippers

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

 

-- typical bin search three insert with 1 quirk:
-- leaf cloner (it replaces a leaf and appends it and a clone to it)
insertOrd :: (Ord a) => a -> TreeZipper a -> TreeZipper a
insertOrd x tz
  = case (if' (x > (val $ tree tz)) goLeft goRight $ tz) of
    Just down -> insertOrd x down
    Nothing -> modifyT (\old -> Node x old old) tz 



isBalanced :: BinTree a -> Bool
isBalanced = snd . heightBalance 
  where    
    heightBalance (Leaf _) = (1, True)
    heightBalance (Node _ l r) = (max lHeight rHeight, lBalance && rBalance && (abs (lHeight - rHeight) <= 1))
      where
        (lHeight, lBalance) = heightBalance l
        (rHeight, rBalance) = heightBalance r


data Infinite a = MinInf | Only a | MaxInf
  deriving (Show, Eq, Ord)

inInterval x  (b, e) = b <= x && x <= e

isBinSTree :: (Ord a) => BinTree a -> Bool
isBinSTree t = isSTree t (MinInf, MaxInf)
  where
    isSTree (Leaf x) interval = inInterval (Only x) interval
    isSTree (Node x l r) i@(b, e) = let infX = (Only x) in (inInterval infX i)
                                  && (isSTree l (b, infX)) && (isSTree r (infX, e))

-- alternative
flatten (Leaf x) = [x]
flatten (Node x l r) = (flatten l) P.++ (x : (flatten r))
isOrdered xs = P.all id $ P.zipWith ( <= ) xs (P.tail xs)
isBinSTree2 = isOrdered . flatten

-- O(n + m)
isSubTree :: (Eq a) => BinTree a -> BinTree a -> Bool
isSubTree t bigT =   DL.isInfixOf (flatten t) (flatten bigT)

binTree1 = Node "A" (Leaf "1") (Node "B" (Leaf "2") (Leaf "3"))
binTree2 = Node "B" (Node "A" (Leaf "1") (Leaf "2"))  (Leaf "3")

binSTree1 = Node 5 (Node 3 (Leaf 1) (Leaf 4))  (Leaf 6)


sublists :: [a] ->[[a]]
sublists xs = P.concatMap (\n -> prefixes $ P.drop n xs) [0.. P.length xs]

prefixes xs = P.map (\n -> DL.take n xs) [1..P.length xs]

root2 = (binTree2, ([], 0 :: Int))
test1 = goBack (fromJust $ return root2 >>= goLeft) (fromJust $ return root2 >>= goRight)
test2 = return root2 >>= goLeft >>= goLeft >>= hitAndRun (\_ -> "wtf") goUncle

