import Data.List
--import Test.QuickCheck
--import Data.Algorithms.KMP
data Automaton a = Node {value   :: a,
                         success :: Automaton a,
                         failure :: Automaton a,
                         accept  :: Bool
                         } | 
                   Null {success :: Automaton a,
                         accept :: Bool}

isNull (Null _ _) = True
isNull _ = False

buildAutomaton :: Eq a => [a] -> Automaton a
buildAutomaton (x:xs) = automaton
  where automaton = Node x (build xs automaton) (Null automaton False) (null xs)
        build [] s = s
        build (x:xs) s
         | x == value s = success s `nextNode` failure s
         | otherwise    = newS      `nextNode` s
         where nextNode a b = Node x (build xs a) b (null xs)
               newS         = success $ until (\s-> isNull s || x == value s) failure s
                         
matchFold :: Eq a => Automaton a -> [a] -> ([a]->b->b) -> ([a]->b->b) -> b -> b
matchFold _ [] _ _ identity = identity
matchFold state text nomat mat identity = match' state text
  where match' _ [] = identity
        match' a (x:xs)
          | not (isNull a) && value a /= x = stay
          | not (accept a)                 = nomat (x:xs) next
          | otherwise                      = mat   (x:xs) next
          where next = match' (success a) xs
                stay = match' (failure a) (x:xs)

isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' pattern text
 | null pattern = True
 | otherwise    = or $ matchFold (buildAutomaton pattern) text (const (False:)) (const (True:)) []

matchTails :: Eq a => [a] -> [a] -> [[a]]
matchTails pattern text
 | null pattern = tails text
 | otherwise    = result
  where result = map ((init pattern)++) $ matchFold (buildAutomaton pattern) text (const id) (:) []

matchIndices :: Eq a => [a] -> [a] -> [Int]
matchIndices pattern text 
 | null pattern = [0..length text]
 | otherwise    = map (\x -> x-n) result
  where n = length pattern
        result = map snd $ filter fst 
                 $ zip (matchFold (buildAutomaton pattern) text (const (False:)) (const (True:)) []) [1..]

{-
prop_isSubstringOf :: [Bool] -> [Bool] -> Bool
prop_isSubstringOf as bs = as `isInfixOf` bs == as `isInfixOf'` bs

kmp as bs = match (build as) bs

prop_isSubstringOf' :: [Int] -> Bool
prop_isSubstringOf' as = cs `kmp` bs == cs `matchIndices` bs
  where cs = map (`rem` 5) as 
        bs = concatMap (const cs) [1..10]
-}