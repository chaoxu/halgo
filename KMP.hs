--import Data.List
--import Test.QuickCheck
--import Data.Algorithms.KMP

isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' pattern text = not $ null $ matchIndices pattern text

matchTails :: Eq a => [a] -> [a] -> [[a]]
matchTails pattern text = map (`drop` text) (matchIndices pattern text)

data Automaton a = Node {value   :: a,
                         success :: Automaton a,
                         failure :: Automaton a,
                         accept  :: Bool
                         } | Null {success :: Automaton a}

isNull (Null _) = True
isNull _ = False

buildAutomaton :: Eq a => [a] -> Automaton a
buildAutomaton xs = automaton
  where automaton = startBuild xs
        startBuild (x:xs) = t 
          where t = Node x (build xs automaton) (Null automaton) (null xs)
                build ys s -- s is: success (previous failure)
                  | null ys      = s
                  | x == value s = s            `nextNode` (failure s)
                  | otherwise    = (failure' s) `nextNode` s
                  where (x,xs)       = (head ys, tail ys)
                        nextNode a b = Node x (build xs (success a)) b (null xs)
                        failure' s
                          | isNull s     = s 
                          | x /= value s = failure' (failure s)
                          | otherwise    = s
                        

matchIndices :: Eq a => [a] -> [a] -> [Int]
matchIndices pattern text
  | null pattern = [0..length text]
  | otherwise    = match' automaton text 1
  where automaton = buildAutomaton pattern
        match' _ [] _ = []
        match' a (x:xs) n
         | not (isNull a) && value a /= x = match' f (x:xs) n
         | isNull a                       = match' s xs (n+1)
         | value a == x && accept a       = [n-length pattern]++match' s xs (n+1)
         | value a == x                   = match' s xs (n+1)
         where s = success a
               f = failure a


{-
prop_isSubstringOf :: [Bool] -> [Bool] -> Bool
prop_isSubstringOf as bs = as `isInfixOf` bs == as `isInfixOf'` bs

kmp as bs = match (build as) bs

prop_isSubstringOf' :: [Int] -> Bool
prop_isSubstringOf' as = cs `kmp` bs == cs `matchIndices` bs
  where cs = map (`rem` 5) as 
        bs = concatMap (const cs) [1..10]
-}