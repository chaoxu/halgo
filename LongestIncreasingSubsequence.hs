import           Control.Arrow  (second)
import           SortedSequence (concatSorted, emptySortedSequence, insert,
                                 removeMin, split, toList)
lis :: Ord a => [a] -> [a]
lis = toList . 
      foldl 
        (\sol x -> uncurry concatSorted $ 
                     second (insert x . removeMin) (split x sol))
        emptySortedSequence
