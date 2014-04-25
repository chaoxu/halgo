import Data.List
import Control.Applicative
import Control.Arrow
import Control.Monad
import RankSelection

type Matrix a = (Int->Int->a, Int, Int)

-- The input is an matrix sorted in both row and column order
-- This selects the kth smallest element. (0th is the smallest)
selectMatrixRank :: Ord a => Int -> Matrix a -> a
selectMatrixRank k (f,n,m)
 | k >= n*m || k < 0 = error "rank doesn't exist"
 | otherwise         = fst $ fst $ biselect k k (f', min n (k+1), min m (k+1))
 where f' x y= (f x y, (x, y))

biselect :: Ord a => Int -> Int -> Matrix a -> (a,a)
biselect lb ub (f',n',m') = join (***) (selectRank values) (lb-ra, ub-ra)
 where mat@(f,n,m)
        | n' > m'   = (flip f', m', n')
        | otherwise = (f', n', m')
       (a, b)
        | n < 3     = (f 0 0, f (n-1) (m-1))
        | otherwise = biselect lb' ub' halfMat
       (lb', ub')   = (lb `div` 2, min ((ub `div` 2) + n) (n * hm - 1))
       (ra, values) = (rankInMatrix mat a, selectRange mat a b)
       halfMat
        | even m = (\x y->f x (if y < hm - 1 then 2 * y else 2 * y - 1), n, hm)
        | odd  m = (\x y->f x (2*y), n, hm)
       hm = m `div` 2 + 1

-- the rank of an element in the matrix
rankInMatrix :: Ord a => Matrix a -> a -> Int
rankInMatrix mat a = sum (map (\(_,y)->1+y) $ frontier mat a)-1

-- select all elements x in the matrix such that a <= x <= b 
selectRange :: Ord a => Matrix a -> a -> a -> [a]
selectRange mat@(f,_,_) a b = concatMap search (frontier mat b)
 where search (x,y) = takeWhile (>=a) $ map (f x) [y,y-1..0]

frontier :: Ord a => Matrix a -> a -> [(Int,Int)]
frontier (f,n,m) b = step 0 (m-1)
 where step i j 
        | i > n-1 || j < 0 = []
        | f i j <= b       = (i,j):step (i+1) j
        | otherwise        = step i (j-1)

-- toString (f, n, m) = show (n,m) ++ show [[f i j|i<-[0..n-1]]|j<-[0..m-1]]
--listVersion (f,n,m) = [f i j|i<-[0..n-1],j<-[0..m-1]]
--trace ("bisecting call "++show lb'++" "++ show ub'++ " elements "++ show a ++ " "++show b ++ toString (half mat)) $ 
                  
mapf f x y= (f x y, (x, y))


matrix :: Matrix Int
matrix = (fff,4,4)
fff:: Int->Int->Int
fff i j = ([[0,1,1,1],
            [1,2,3,4],
            [3,3,4,5],
            [10,11,12,13]]!!i)!!j

