{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
module SortedSequence (SortedSequence, MeasuredOrd(..), emptySortedSequence, toList, split, insert, removeMin, concatSorted) where
import           Data.FingerTree (FingerTree, Measured (..), ViewL (..), empty,
                                 viewl, (<|), (><))
import qualified Data.FingerTree as F (null, split)
import           Data.Monoid     (Monoid (..))

data MeasuredOrd a = Min | MeasuredOrd a deriving (Eq, Ord, Show)

instance Ord a => Monoid (MeasuredOrd a) where
    mappend (MeasuredOrd a) (MeasuredOrd b) = MeasuredOrd (max a b)
    mappend Min (MeasuredOrd a) = MeasuredOrd a
    mappend (MeasuredOrd a) Min = MeasuredOrd a
    mappend Min Min = Min
    mempty = Min
instance Ord v => Measured (MeasuredOrd v) (MeasuredOrd v) where
    measure = id

type SortedSequence a = FingerTree (MeasuredOrd a) (MeasuredOrd a)

emptySortedSequence :: Ord a => SortedSequence a
emptySortedSequence = empty

toList :: Ord a => SortedSequence a -> [a]
toList xs
 | F.null xs   = []
 | otherwise = x:toList xs'
 where (MeasuredOrd x :< xs') = viewl xs

split :: Ord a => a -> SortedSequence a -> (SortedSequence a, SortedSequence a)
split x = F.split (>= MeasuredOrd x)

removeMin :: Ord a => SortedSequence a -> SortedSequence a
removeMin ss 
 | F.null ss = ss
 | otherwise = ss'
  where (_ :< ss') = viewl ss

insert :: Ord a => a -> SortedSequence a -> SortedSequence a
insert x ss = smaller >< (MeasuredOrd x <| larger)
  where (smaller,larger) = split x ss

concatSorted :: Ord a => SortedSequence a -> SortedSequence a -> SortedSequence a
concatSorted a b = a >< b
