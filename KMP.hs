import Data.List

isInfixOf' :: Eq a => [a] -> [a] -> Bool
isInfixOf' pattern text = not $ null $ matchIndices pattern text

matchIndices :: Eq a => [a] -> [a] -> [Int]
matchIndices pattern text
 | null pattern = [0..length text]
 | otherwise    = indices (buildAutomaton pattern) 1 text
 where n = length pattern
       indices _ _ [] = []
       indices a i (x:xs)
        | value a /= x && isNull f = indices a (i+1) xs
        | value a /= x             = indices f i (x:xs)
        | accept a                 = (i-n):indices s (i+1) xs
        | otherwise                = indices s (i+1) xs
        where f = failure a
              s = success a

matchTails :: Eq a => [a] -> [a] -> [[a]]
matchTails pattern text = map (`drop` text) (matchIndices pattern text)

data Automaton a = Node {value   :: a,
                         success :: Automaton a,
                         failure :: Automaton a,
                         border  :: Automaton a,
                         accept  :: Bool
                         } | Null

isNull Null = True
isNull _ = False

buildAutomaton :: Eq a => [a] -> Automaton a
buildAutomaton xs = automaton
  where automaton = startBuild xs
        startBuild (x:xs) = t 
          where t = Node x (buildAutomaton' xs t) Null Null (null xs)
        buildAutomaton' ys ps
          | null ys   = currentBorder
          | otherwise = currentState
          where (x,xs) = (head ys, tail ys)
                currentState   = Node x (buildAutomaton' xs currentState) currentFailure currentBorder (null xs)
                currentBorder  = findBorder  prevBorder
                currentFailure = findFailure prevBorder
                prevValue      = value ps
                prevBorder     = border ps
                currentValue   = x
                findBorder     = findState border  (const False)
                findFailure    = findState failure (==currentValue)
                findState kind switch state
                  | isNull state             = automaton
                  | value state == prevValue = if switch (value s) then recurse f else s
                  | otherwise                = recurse f
                  where recurse = findState kind switch
                        s = success state
                        f = kind state


prop_isSubstringOf :: [Bool] -> [Bool] -> Bool
prop_isSubstringOf as bs = as `isInfixOf` bs == as `isInfixOf'` bs