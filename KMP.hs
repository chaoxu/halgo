--import Data.List
--import Test.QuickCheck
--import Data.Algorithms.KMP
data Automaton a = Node {value   :: a,
                         success :: Automaton a,
                         failure :: Automaton a,
                         accept  :: Bool
                         } | 
                   Null {success :: Automaton a}

isNull (Null _) = True
isNull _ = False

buildAutomaton :: Eq a => [a] -> Automaton a
buildAutomaton xs = automaton
  where automaton = startBuild xs
        startBuild (x:xs) = t 
          where t = Node x (build xs automaton) (Null automaton) (null xs)
                build ys s -- s is: success (previous failure)
                  | null ys      = s
                  | x == value s = s          `nextNode` failure s
                  | otherwise    = failure' s `nextNode` s
                  where (x,xs)       = (head ys, tail ys)
                        nextNode a b = Node x (build xs (success a)) b (null xs)
                        failure' s
                          | isNull s     = s 
                          | x /= value s = failure' (failure s)
                          | otherwise    = s

matchFold :: Eq a => Automaton a -> [a] -> ([a]->b->b) -> ([a]->b->b) -> b -> b
matchFold _ [] _ _ identity = identity
matchFold state text nomat mat identity = match' state text
  where match' _ [] = identity
        match' a (x:xs)
          | not (isNull a) && value a /= x = stay
          | isNull a                       = nomat (x:xs) next
          | value a == x && accept a       = mat   (x:xs) next
          | value a == x                   = nomat (x:xs) next
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
  where result = map (pattern++) $ matchFold (buildAutomaton pattern) text (const id) (:) []

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