module Utils (equivalentClasses) where

equivalentClasses :: (a->a->Bool)->[a]->[[a]]
equivalentClasses eq = foldl parts []
  where parts [] a = [[a]]
        parts (x:xs) a
         | eq (head x) a = (a:x):xs
         | otherwise     = x:parts xs a