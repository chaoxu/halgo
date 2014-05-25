import           Data.Function   (on)
import qualified Data.Graph      as G (buildG, reachable)
import           Data.List       (find, isPrefixOf)
import qualified Data.Map.Lazy   as Map (Map, fromList, (!))
import           Data.Maybe      (fromMaybe, isNothing)
import           Data.SuffixTree (STree)
import qualified Data.SuffixTree as T (Prefix, STree (..), constructWith,
                                       findEdge, fold, mkPrefix, prefix)
import           Trie            (Trie (..), trieFrom)
import           Utils           (equivalentClasses)

-- This implements the Sardinas–Patterson algorithm. It takes O(nk) time to test
-- if a set of strings is uniquely decodable.

-- The general alphabet contains the original alphabet and also a few new
-- alphabets, which will be used to represent where it ends a code word
data GeneralAlphabet a = Char a | Index Int deriving (Eq, Show)
type Suffix = (Int, Int)

-- build a generalized suffix tree. The suffix tree package doesn't
-- support it :( Use the standard find suffix tree of the concadination
-- trick.
generalizedSuffixTree :: Eq a => [[a]] -> STree (GeneralAlphabet a)
generalizedSuffixTree xs = T.fold reset id fprefix reset leaf st
  where st = T.constructWith (map head (equivalentClasses (==) s)) s
        s  = concat $ zipWith (\x y-> map Char x ++ [Index y]) xs [0..]
        leaf = T.Leaf
        reset = const leaf
        fprefix p t T.Leaf = T.Node [(cutPrefix p,t)]
        fprefix p t (T.Node edges) = T.Node ((cutPrefix p,t):edges)
        cutPrefix p = p'
          where (a,b) = break isIndex (T.prefix p)
                p' -- = T.mkPrefix $ takeWhile notIndex (T.prefix p)
                 | null b    = p
                 | otherwise = T.mkPrefix (a ++ [head b])

-- Input a suffix tree for the code words and one single code word c
-- It returns all the suffix in the suffix tree that is a prefix of c
suffixFind :: Eq a => STree (GeneralAlphabet a) -> [a] -> [Suffix]
suffixFind st cw = suffixFind' st cw 0
 where 
    suffixFind' T.Leaf _ _ = []
    suffixFind' (T.Node edges) xs' n -- n is the length of transversed
      | null xs'  = suff
      | otherwise = suff ++ maybe [] findcand cand
      where cand = find (\edge -> Char x == fstchar edge) edges
            findcand (pre, nextNode)
             | null remain = suffixFind' nextNode xs (n+1)
             | otherwise   = suffixFind' (T.Node [(T.mkPrefix remain,nextNode)]) xs (n+1)
             where remain = tail (T.prefix pre)
            (x,xs) = (head xs', tail xs')
            suff   = map ((\(Index y)-> (y,n)) . fstchar) candidate
              where candidate = filter (isIndex . fstchar) edges

-- Input a suffix tree for the code words and one single code word c
-- It returns all the suffix in the suffix tree that contains c as a prefix
prefixFind :: Eq a => STree (GeneralAlphabet a) -> [a] -> [Suffix]
prefixFind st c
 | isNothing (T.findEdge (map Char c) st) = []
 | otherwise = suffixes (T.Node [(T.mkPrefix $ drop removal $ T.prefix pre,node)])
 where (Just ((pre,node), removal)) = T.findEdge (map Char c) st

-- Find all the suffixes in the tree in the Suffix format
suffixes :: STree (GeneralAlphabet a) -> [Suffix]
suffixes st = map (\(x, Just (Index y))-> (y,x-1)) solve
 where solve   = T.fold (const []) id fprefix (const [(0,Nothing)]) [] st
       fprefix p t v = map (\(x,y)-> (x+length pre, Just (fromMaybe (last pre) y))) t ++ v
        where pre = T.prefix p

-- first character in the edge
fstchar :: (T.Prefix a, b) -> a
fstchar edge = head $ T.prefix $ fst edge

-- check if it is a index in the generalized alphabet
isIndex :: GeneralAlphabet t -> Bool
isIndex (Index _) = True
isIndex _         = False

-- this builds the graph
buildAdjacencyList :: Eq a => [[a]] -> [(Int,Int)]
buildAdjacencyList cws = concat (map suf cws2 ++ map pre cws) ++ start
  where st    = generalizedSuffixTree cws
        h a b = table Map.! (a,b)
        table = buildHash cws
        -- add edge (a,b) when ab = c
        suf (x,i) = map (\(ind,l) -> (h ind l, h i (n-l))) (suffixFind st x)
          where n = length x
        -- add edge (a,b) when cb = a
        pre x     = map (\(ind,l) -> (h ind (n+l), h ind l)) (prefixFind st x)
          where n = length x
        cws2 = zip cws [0..]
        initNodes = [(j,length y - length x)| x<-cws, (y,j)<-cws2, x/=y, x `isPrefixOf` y]
        start     = [(1 + sum (map length cws), h x y)|(x,y)<-initNodes]

-- maps suffix that represent the same string to the same integer
buildHash :: Eq a => [[a]] -> Map.Map (Int,Int) Int
buildHash cws = Map.fromList table
  where table = concatMap (\(xs,y)-> map (\x->(x,y)) xs) $ zip from [0..]
        cws2 = zipWith (\x y-> map (\t-> (t,y)) x) (map reverse cws) [0..]
        trie = trieFrom (on (==) fst) cws2
        foldtree (Node char xs) n = map (\y->(snd y,n)) char : concatMap (\x->foldtree x (n+1)) xs
        from = zip [0..length cws] (repeat 0) : tail (foldtree trie 0)

-- build a graph and check if one can reach 0 (epsilon) from initial vertices
sardinasPatterson :: Eq a => [[a]] -> Bool
sardinasPatterson cws = 0 `notElem` G.reachable graph start
  where graph = G.buildG (0,start) (buildAdjacencyList cws)
        start = 1 + sum (map length cws)

-- test with some samples
test :: Bool
test = [False,True,False,True,True] == map sardinasPatterson [test1,test2,test3,test4,test6] 
  where test1 = ["ab","abba","baaabad","aa","badcc","cc","dccbad","badba"]
        test2 = ["ab","a"]
        test3 = ["1", "011", "01110", "1110", "10011"]
        test4 = ["0", "01", "011"]
        test6 = ["0", "10", "110", "111"]