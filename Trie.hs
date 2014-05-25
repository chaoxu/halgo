module Trie (Trie(Node), trieFrom) where
import           Data.Function (on)
import           Utils (equivalentClasses)

data Trie a = Node [a] [Trie a] deriving (Eq, Show)

trieFrom :: (a->a->Bool)->[[a]]->Trie a
trieFrom eq strings = Node [] (trieFrom' eq strings)

trieFrom' :: (a->a->Bool)->[[a]]->[Trie a]
trieFrom' eq strings = map newNode nodes
 where cand  = filter (not.null) strings
       nodes = equivalentClasses (on eq head) cand
       newNode xs = Node (map head xs) (trieFrom' eq (map tail xs))
