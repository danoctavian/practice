{-# LANGUAGE RankNTypes #-}

module LeetCodeOJ where

import Utils
import Data.List as DL
import Data.Function
import Data.Maybe
import Data.Vector as DV
import Data.IntTrie
import Data.Monoid

-- Merge intervals

mergeIntervals :: (Ord a) => [(a, a)] -> [(a, a)]
mergeIntervals [] = []
mergeIntervals is = DL.reverse $ DL.foldl
                    (\(m : ms) newI -> if' (overlap m newI)
                                         ((fst m, snd newI) : ms)
                                         (newI : m : ms))
                    [DL.head is] (DL.tail is)
                    
overlap i1 i2 = y >= a where [(x, y), (a, b)] = DL.sort [i1, i2]


-- palindrome partitioning
-- sarah palin

--palinParts
-- maybeToList

type Parts a = [a] -> [[[a]]]

isPalin xs = (xs == DL.reverse xs) 

-- memoized version
palinParts :: Eq a => Parts a -> Parts a
palinParts f [x] = [[[x]]]
palinParts f xs
  = mergeAll $ catMaybes $ [let (p, rest) = DL.splitAt i xs in
                   if' (isPalin p)
                      (Just $ DL.map (p :) (f rest) )
                      Nothing
    | i <- [1..(DL.length xs - 1)]]
    where
      mergeAll = DL.foldl (DL.++) []


t :: IntTrie String
t = overwrite (0 :: Int) "wtf" mempty
r = apply t (0 :: Int)


-- memoizing for a particular string
memS :: Eq a => [a] -> Parts a -> Parts a
memS s f word = (DV.fromList $ DL.map (\i -> f (DL.drop (ls - i) s)) [1..(DL.length s)]) ! (DL.length word - 1)
  where
    ls = DL.length s
palinPartsMemo s = fix ((memS s) . palinParts) $ s 


-- speed test (covers eyes)


{-
palinParts :: (Eq a) => Vector a -> [[Vector a]]
palinParts [x] = [[[x]]]
palinParts xs
  = mergeAll $ catMaybes $ [let (p, rest) = DV.splitAt i xs in
                   if' (isPalin p)
                      (Just $ DV.map (p :) (palinParts rest) )
                      Nothing
    | i <- [1..(DV.length xs - 1)]]
    where
      mergeAll = DV.foldl (DV.++) DV.empty
-}      

