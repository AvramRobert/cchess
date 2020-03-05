module Lib.Coll (first, spread, every, oneOf, conjoin, consume, keepLast, zipped, when, once) where

import Data.List (find)
import Data.Maybe (isJust)

data Action = Continue | Interrupt

spread :: [a -> b] -> a -> [b]
spread fs a = [f a | f <- fs]

conjoin :: [a -> [b]] -> a -> [b]
conjoin fs a = fs >>= (\f -> f a) 

every :: [a -> Bool] -> a -> Bool
every (p : ps) a = p a && every ps a
every [] _       = True

oneOf :: [(a -> Bool)] -> a -> Bool
oneOf [] a = True
oneOf fs a = isJust $ find (\f -> f a) fs

when :: (a -> Bool) -> (a -> b) -> (a -> Bool, a -> b, Action) 
when p f = (p, f, Continue)

once :: (a -> Bool) -> (a -> b) -> (a -> Bool, a -> b, Action)
once p f = (p, f, Interrupt)

consume :: [(a -> Bool, a -> b, Action)] -> [a] -> [b]
consume conds []     = []
consume conds (a:as) = case (find (\(p, _, _) -> p a) conds) of
    (Just (_, f, Continue))  -> (f a) : (consume conds as)
    (Just (_, f, Interrupt)) -> (f a) : []
    (Nothing)                -> []

zipped :: (a -> [b]) -> (a -> [c]) -> a -> [(b, c)]
zipped f g a = zip (f a) (g a)  

keepLast :: [a] -> [a]
keepLast []     = []
keepLast (a:[]) = (a:[])
keepLast (a:as) = keepLast as

first :: [a] -> Maybe a
first []    = Nothing
first (a:_) = Just a