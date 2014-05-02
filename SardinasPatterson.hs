import Data.List
import Data.SuffixTree

--work in progress

-- This implements the Sardinas–Patterson algorithm. It takes O(nk) time to test
-- if a set of strings is uniquely decodable.
data GeneralAlphabet a = Char a | Index Int deriving (Eq, Show)

generalizedSuffixTree xs = constructWith (nub s) s
  where s = concat $ zipWith (\x y-> map Char x ++ [Index y]) xs [0..]
--uniquelyDecodable xs = 
--  where stree = constructWith xs
