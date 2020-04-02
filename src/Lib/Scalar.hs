module Lib.Scalar (cmap) where

cmap :: [a -> a] -> a -> a
cmap xs = (foldl (.) id xs)  