module SetTalk where

import           Control.Applicative            ( liftA2 )

type T = Bool

-- Function for shifting from set to it's characteristic function
toCharFunc :: Eq a => [a] -> a -> T
toCharFunc xs x = x `elem` xs

-- Function for shifting from a set of pairs to a curried relation
toFunc2 :: (Eq a, Eq b) => [(a, b)] -> b -> a -> T
toFunc2 = flip . curry . toCharFunc

-- Function for shifting from a set of sets to a GQ.
toGQ :: Eq a => [[a]] -> (a -> T) -> T
toGQ q f = toSet (concat q) f `elem` q

-- powerset function
powerset :: [a] -> [[a]]
powerset []       = [[]]
powerset (x : xs) = powerset xs ++ map (x :) (powerset xs)

-- A function from a generalized quantifier q to a set of sets (given a domain dom).
-- note that this implementation only works with finite domains.
toSetOfSets :: Eq a => [a] -> ((a -> T) -> T) -> [[a]]
toSetOfSets dom q = [ xs | xs <- powerset dom, q (toCharFunc xs) ]

-- a function from a predicate and a domain, to the graph of the predicate
toGraph :: [a] -> (a -> T) -> [(a, T)]
toGraph dom f = [ (x, f x) | x <- dom ]

-- a function from a predicate and a domain, to the set the predicate characterises relative to the domain.
toSet :: [a] -> (a -> T) -> [a]
toSet dom f = [ x | (x, True) <- toGraph dom f ]

-- a function from two domains and a two-place predicate, to the graph of the predicate.
toGraph2 :: [a] -> [b] -> (b -> a -> T) -> [((b, a), T)]
toGraph2 domInt domExt f = [ (x, uncurry f x) | x <- liftA2 (,) domExt domInt ]

-- A function from two domains and a two-place predicate, to the set of pairs the predicate characterises relative to the domain.
toSet2 :: [a] -> [b] -> (b -> a -> T) -> [(b, a)]
toSet2 domInt domExt f = [ x | (x, True) <- toGraph2 domInt domExt f ]
