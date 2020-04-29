module Lib.Coll (first, spread, every, conjoin, consume, keepLast, mapFirst, 
                 zipped, when, once, exactly, groupOn, asListOf, chunksOf, maxBy) where

import Debug.Trace (trace)
import Control.Monad (foldM)
import Data.List (find, groupBy, sortOn)
import Data.Maybe (isJust)

mapFirst :: [a -> Maybe b] -> a -> Maybe b
mapFirst [] _     = Nothing
mapFirst (f:fs) a = maybe (mapFirst fs a) Just (f a)

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . map (take n) . iterate (drop n)

asListOf :: Foldable f => (a -> b) -> f a -> [b]
asListOf f = foldr (\a xs -> (f a) : xs) []

groupOn :: (Eq a, Ord a) => (b -> a) -> [b] -> [(a, [b])]
groupOn f = keep . groupBy (\a b -> f a == f b) . sortOn f
    where keep [] = []
          keep ([]:as) = keep as
          keep (a:as)  = (f $ head a, a) : keep as

spread :: [a -> b] -> a -> [b]
spread fs a = [f a | f <- fs]

conjoin :: [a -> [b]] -> a -> [b]
conjoin fs a = fs >>= (\f -> f a) 

every :: [a -> Bool] -> a -> Bool
every (p : ps) a = p a && every ps a
every [] _       = True

first :: [a] -> Maybe a
first []    = Nothing
first (a:_) = Just a

zipped :: (a -> [b]) -> (a -> [c]) -> a -> [(b, c)]
zipped f g a = zip (f a) (g a)  

maxBy :: (Ord c, Foldable f) => (b -> c) -> f b -> Maybe b
maxBy f = foldr compare Nothing
    where compare b Nothing                = Just b
          compare b (Just b') | f b > f b' = Just b
          compare b b'                     = b' 

-- This function expects the list to be reversed, i.e. the last element to be the head
keepLast :: [a] -> [a]
keepLast []    = []
keepLast (a:_) = a:[]

data Action = Continue | Interrupt | Dismiss deriving (Show, Eq)

when :: (a -> Bool) -> (a -> b) -> (a -> Bool, a -> b, Action) 
when p f = (p, f, Continue)

once :: (a -> Bool) -> (a -> b) -> (a -> Bool, a -> b, Action)
once p f = (p, f, Interrupt)

exactly :: (a -> Bool) -> (a -> b) -> (a -> Bool, a -> b, Action)
exactly p f = (p, f, Dismiss)

gobble :: [(a -> Bool, a -> b, Action)] -> a -> (Action, Maybe b)
gobble [] a     = (Interrupt, Nothing)
gobble (g:gs) a = case g of
            (p, f, Continue)  | p a -> (Continue,  Just $ f a)
            (p, f, Interrupt) | p a -> (Interrupt, Just $ f a)
            (p, f, Dismiss)   | p a -> (Dismiss,   Just $ f a)
            (p, f, Dismiss)         -> (Dismiss,   Nothing)
            _                       -> gobble gs a

consume :: [(a -> Bool, a -> b, Action)] -> [a] -> [b]
consume conds as = foldl gather id as [] 
    where gather f a xs = case (gobble conds a) of
            (Continue, Just x)  -> xs `seq` f (x : xs)  
            (Interrupt, Just x) -> x : xs
            (Dismiss, Just x)   -> xs `seq` f (x : xs) 
            (Dismiss, Nothing)  -> []
            _                   -> xs 