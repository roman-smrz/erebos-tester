module Util where

uniq :: Eq a => [a] -> [a]
uniq (x:y:xs) | x == y    = uniq (x:xs)
              | otherwise = x : uniq (y:xs)
uniq xs = xs

uniqOn :: Eq b => (a -> b) -> [a] -> [a]
uniqOn f (x:y:xs) | f x == f y = uniqOn f (x:xs)
                  | otherwise  = x : uniqOn f (y:xs)
uniqOn _ xs = xs

andM :: (Foldable t, Monad m) => t (m Bool) -> m Bool
andM = foldr (\a b -> a >>= \case True -> b; False -> return False) (return True)

allM :: (Foldable t, Monad m) => (a -> m Bool) -> t a -> m Bool
allM f = foldr (\a b -> f a >>= \case True -> b; False -> return False) (return True)
