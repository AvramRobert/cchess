module UCI.Interface where

import qualified System.Process as P
import qualified GHC.IO.Handle as H

eval :: String -> IO String
eval = P.readProcess "/home/robert/Downloads/stockfish/Linux/stockfish" ["go", "depth", "10"]