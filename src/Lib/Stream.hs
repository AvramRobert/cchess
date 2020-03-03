module Lib.Stream (FTag, while, exactly, accept, allow, ignore, terminate, wye, lastly, contraSieve) where

import Lib.Coll (conjoin)

data FTag a b = Allowed  a |
                Ignored  a |
                Accepted b |
                Terminated b

while :: (a -> Bool) -> [FTag a b] -> [FTag a b]
while p []       = []
while p (ft:fts) = case ft of 
    (Allowed a) | p a -> (Allowed a) : (while p fts)
    (Allowed a)       -> (Ignored a) : (while p fts)
    _                 -> ft : (while p fts)

exactly :: (a -> Bool) -> [FTag a b] -> [FTag a b]
exactly p [] = []
exactly p (ft:fts) = case ft of
    (Allowed a) | p a -> (Allowed a) : (exactly p fts)
    (Allowed a)       -> []
    _                 -> ft : (exactly p fts)

allow :: [a] -> [FTag a b]
allow = map Allowed

ignore :: [a] -> [FTag a b]
ignore = map Ignored

accept :: (a -> b) -> [FTag a b] -> [FTag a b]
accept f []       = []
accept f (ft:fts) = case ft of (Allowed a) -> (Accepted (f a))  : (accept f fts)
                               (Ignored a) -> (Allowed a)       : (accept f fts)
                               _           -> ft                : (accept f fts)

terminate :: (a -> b) -> [FTag a b] -> [FTag a b]
terminate f []     = []
terminate f (ft:_) = case ft of (Allowed a) -> (Terminated (f a)) : []
                                _           -> []

wye :: (a -> [FTag b e]) -> (a -> [FTag c f]) -> a -> [FTag a (e, f)]
wye h y a = spread (h a) (y a)
    where spread [] _ = []
          spread _ [] = []
          spread (b:bs) (c:cs) = case (b, c) of (Accepted e, Accepted f)   -> Accepted (e, f) : (spread bs cs)
                                                (Accepted e, Terminated f) -> Terminated (e, f) : []
                                                (Terminated e, Accepted f) -> Terminated (e, f) : []
                                                _                          -> spread bs cs

lastly :: ([FTag a b] -> [FTag a b]) -> [FTag a b] -> [FTag a b]
lastly f []       = []
lastly f (ft:[])  = f (ft:[])
lastly f (ft:fts) = ft:(lastly f fts)

contraSieve :: (b -> Bool) -> [a -> [FTag c b]] -> a -> [b]
contraSieve p fs = gather . conjoin fs
      where gather []     = []
            gather (d:ds) = case d of (Accepted b)   | p b -> b : (gather ds)
                                      (Terminated b) | p b -> b : (gather ds)
                                      _                    -> gather ds

instance Functor (FTag a) where
    fmap f (Accepted a)   = Accepted $ f a
    fmap f (Terminated a) = Terminated $ f a
    fmap f (Ignored b)    = Ignored b
    fmap f (Allowed b)    = Allowed b