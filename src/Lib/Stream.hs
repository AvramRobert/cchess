module Lib.Stream (while, accept, terminate, contraSieve) where

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


accept :: (a -> b) -> [FTag a b] -> [FTag a b]
accept f []       = []
accept f (ft:fts) = case ft of (Allowed a) -> (Accepted (f a))  : (accept f fts)
                               (Ignored a) -> (Allowed a)       : (accept f fts)
                               _           -> ft                : (accept f fts)

terminate :: (a -> b) -> [FTag a b] -> [FTag a b]
terminate f []     = []
terminate f (ft:_) = case ft of (Allowed a) -> (Terminated (f a)) : []
                                _           -> []

contraSieve :: (b -> Bool) -> [a -> [FTag c b]] -> a -> [b]
contraSieve p fs = gather . conjoin fs
      where gather []     = []
            gather (d:ds) = case d of (Accepted b)   | p b -> b : (gather ds)
                                      (Terminated b) | p b -> b : (gather ds)
                                      _                    -> gather ds