module Lib.PipedStream (Pipe, tap, while, exactly, accept, terminate, keepLast, mapTo, sink, fork) where

import Lib.Coll (conjoin)

data Pipe a b = Allowed  a |
                Ignored  a |
                Accepted b

while :: (a -> Bool) -> [Pipe a b] -> [Pipe a b]
while p []       = []
while p (ft:fts) = case ft of 
    (Allowed a) | p a -> (Allowed a) : (while p fts)
    (Allowed a)       -> (Ignored a) : (while p fts)
    _                 -> ft : (while p fts)

exactly :: (a -> Bool) -> [Pipe a b] -> [Pipe a b]
exactly p [] = []
exactly p (ft:fts) = case ft of
    (Allowed a) | p a -> (Allowed a) : (exactly p fts)
    (Allowed a)       -> []
    _                 -> ft : (exactly p fts)

tap :: [a] -> [Pipe a b]
tap = map Ignored

accept :: (a -> b) -> [Pipe a b] -> [Pipe a b]
accept f []       = []
accept f (ft:fts) = case ft of (Allowed a) -> (Accepted (f a))  : (accept f fts)
                               (Ignored a) -> (Allowed a)       : (accept f fts)
                               _           -> ft                : (accept f fts)

terminate :: (a -> b) -> [Pipe a b] -> [Pipe a b]
terminate f []     = []
terminate f (ft:_) = case ft of (Allowed a) -> (Accepted (f a)) : []
                                _           -> []

fork :: (a -> [Pipe h b]) -> (a -> [Pipe h c]) -> a -> [Pipe h (b, c)]
fork f g a = merge (f a) (g a)
    where merge [] _                  = []
          merge _ []                  = []
          merge (bpi:bpps) (cpi:cpps) = case (bpi, cpi) of 
              (Accepted b, Accepted c) -> (Accepted (b, c)) : (merge bpps cpps)
              _                        -> merge bpps cpps

mapTo :: (b -> c) -> [Pipe a b] -> [Pipe a c]
mapTo f []       = []
mapTo f (pi:pps) = case pi of (Accepted b) -> (Accepted $ f b) : (mapTo f pps)
                              (Ignored a)  -> (Ignored a) : (mapTo f pps)
                              (Allowed a)  -> (Allowed a) : (mapTo f pps)

keepLast :: [Pipe a b] -> [Pipe a b]
keepLast  []    = []
keepLast (a:[]) = a:[]
keepLast (_:as) = keepLast as

sink :: [Pipe a b] -> [b]
sink []     = []
sink (a:as) = case a of (Accepted b) -> b : (sink as)
                        _            -> sink as