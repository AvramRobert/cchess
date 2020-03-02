module Lib.Stream (FTag, while, lastly, accept, allow, ignore, terminate, contraSieve) where

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