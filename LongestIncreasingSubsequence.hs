import           SortedSequence  (emptySortedSequence, toList, split, insert, removeMin, concatSorted)
import           Control.Arrow   (second)
lis :: Ord a => [a] -> [a]
lis = toList . foldl f emptySortedSequence
 where f sol x = uncurry concatSorted (second (insert x . removeMin) (split x sol))