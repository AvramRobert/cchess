module Lib.Bench (time) where

import Text.Printf
import System.CPUTime (getCPUTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO.Unsafe (unsafePerformIO)

dotime :: String -> (() -> a) -> IO a
dotime name f = do 
        t0 <- getCPUTime
        a  <- return $ f ()
        t1 <- getCPUTime
        let diff = (fromIntegral (t1 - t0))  / (10^9)
        printf "%s: %0.4f msecs\n" (name :: String) (diff :: Double)
        return a

time :: String -> (() -> a) -> a
time name = unsafePerformIO . dotime name   