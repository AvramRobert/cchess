{-# LANGUAGE GADTs #-}

module Lib.Freer where

data Freer f a where
    Value  :: a -> Freer f a
    Effect :: (f x) -> (x -> Freer f a) -> Freer f a

instance Functor (Freer f) where
    fmap f (Value a)     = Value $ f a
    fmap f (Effect fx g) = Effect fx (fmap f . g) 

instance Applicative (Freer f) where
    pure = Value
    (<*>) (Value f) (Value a)             = Value $ f a
    (<*>) fab (Effect fx f)               = Effect fx (\x -> fab <*> (f x))
    (<*>) (Effect fx f) fa                = Effect fx (\x -> (f x) <*> fa)

instance Monad (Freer f) where
    (>>=) (Value a) f      = f a
    (>>=) (Effect fx ff) f = Effect fx (\x -> (ff x) >>= f) 

perform :: f a -> Freer f a
perform fa = Effect fa Value