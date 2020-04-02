module Lib.Scalar (cmap) where

cmap :: [a -> a] -> a -> a
cmap fs a = foldl (\a' f -> f a') a fs