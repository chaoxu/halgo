import Data.Tree
import Data.Maybe
{-
treeAllChildrenSame :: Eq a => Tree a -> Tree Bool
treeAllChildrenSame (Node _ children)
 | null children = Node True []
 | otherwise     = Node (all fst result) (map snd result)
  where (first,rest) = (head children, tail children)
        result = map (match first) rest
        match :: Eq a => Tree a -> Tree a -> (Bool, Tree Bool)
        match (Node x c) (Node y c') = (alleq && (x == y), Node alleq newchild)
         where matchall = zipWith match c c'
               alleq  = all fst matchall && (length c == length c')
               newchild  = map snd matchall
-}

-- this builds a new tree such that the vertex v is true
-- if and only if it has a parent, and it's first sibling equals it
treeSameAsFirstSibling :: Eq a => Tree a -> Tree Bool
treeSameAsFirstSibling t = match t Nothing

match :: Eq a => Tree a -> Maybe (Tree a) -> Tree Bool
match (Node x c) pattern = Node (maybe False (const alleq) pattern) newchild
  where newchild = zipWith match c (map Just c'++ repeat Nothing)
        alleq    = all rootLabel newchild && (x == y)
        (Node y c') = fromJust pattern