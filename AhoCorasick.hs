import Data.List
import Data.Maybe
import Data.Monoid
import Data.Function
import Control.Arrow
data Automaton a b = Node {delta  :: a -> Automaton a b,
                           output :: b
                         }

equivalentClasses :: (a->a->Bool)->[a]->[[a]]
equivalentClasses eq = foldl parts []
  where parts [] a = [[a]]
        parts (x:xs) a 
         | eq (head x) a = (a:x):xs
         | otherwise     = x:parts xs a

buildAutomaton :: (Monoid b,Eq a) => [([a],b)] -> Automaton a b
buildAutomaton xs = automaton
  where automaton = build (const automaton) xs mempty

build :: (Monoid b,Eq a)=> (a -> Automaton a b) -> [([a],b)] -> b -> Automaton a b
build trans' xs out = node
  where node  = Node trans out
        trans a
          | isNothing next = trans' a
          | otherwise      = fromJust next
          where next = lookup a table
        equiv = equivalentClasses (on (==) (head . fst)) xs
        table =  map transPair equiv
        transPair xs = (a, build (delta (trans' a)) ys out)
         where a  = head $ fst $ head xs
               (ys,zs) = partition (not . null . fst) $ map (first tail) xs
               out = mappend (mconcat $ map snd zs) (output $ trans' a)

match :: Eq a => Automaton a b -> [a] -> [b]
match a xs = map output $ scanl delta a xs

match' :: Eq a => [[a]] -> [a] -> [[[a]]]
match' pat = match (buildAutomaton $ map (\x-> (x,[x])) pat)

isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' xs ys = getAll $ mconcat $ match (buildAutomaton [(xs, All True)]) ys
