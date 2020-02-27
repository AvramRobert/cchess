module Lib (xor, spread, spreadM, every, oneOf, keepUntil) where

import Data.List (find)
import Data.Maybe (isJust)

xor :: Bool -> Bool -> Bool
xor a b = a /= b

spread :: [a -> b] -> a -> [b]
spread fs a = [f a | f <- fs]

spreadM :: [a -> Maybe b] -> a -> [b]
spreadM [] a       = []
spreadM (f : fs) a = case (f a) of (Just b)  -> b : (spreadM fs a)
                                   (Nothing) -> spreadM fs a

every :: [a -> Bool] -> a -> Bool
every (p : ps) a = p a && every ps a
every [] _       = True

oneOf :: [(a -> Bool)] -> a -> Bool
oneOf [] a = True
oneOf fs a = isJust $ find (\f -> f a) fs

keepUntil :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
keepUntil stop keep (a : as) | keep a && stop a = a : []
keepUntil stop keep (a : as) | keep a = a : (keepUntil stop keep as)
keepUntil stop keep (a : as) | stop a = a : []
keepUntil _ _ _                       = []