import Data.List (group, sort)
import Data.Dequeue hiding (last)
import qualified Data.Dequeue as D
import Data.Monoid
import Data.Ord
import Data.Maybe 
-- This solves the ksum problem
-- it can be generalized to ordered monoids I think

-- This is a modified version, where we have a target sum
-- This works for any commutative monoid
ksum :: (Monoid a, Ord a) => Int -> a -> [a]->Bool
ksum k n xs
 | even k    = solve sums mempty
 | otherwise = any (solve sums) xs
 where sums = bankersDequeue $ sort $ zipCartesian mconcat (k `div` 2) xs
       solve sums t = solve' sums
         where solve' sums
                | isNothing (first sums)  = False
                | v < n                   = solve' (snd $ popFront sums)
                | v > n                   = solve' (snd $ popBack sums)
                | otherwise               = True
                where v = mconcat [fromJust (first sums),fromJust (D.last sums),t]

zipCartesian :: ([a] -> b) -> Int -> [a] -> [b]
zipCartesian f k xs = map f $ sequence (replicate k xs)

bankersDequeue :: [a] -> BankersDequeue a 
bankersDequeue xs = fromList xs