{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}
module RedBlackTree where

import Prelude as P
import Control.Monad.ST
import Data.Vector.Mutable as DVM
import Data.Vector as DV
import Control.Monad as CM
import Control.Monad.Primitive
import Data.Maybe

import Utils
import BinTree
-- as specified by http://en.wikipedia.org/wiki/Red%E2%80%93black_tree

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
