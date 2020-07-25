module UCI.Interface where

import qualified System.Process as P
import qualified GHC.IO.Handle as H

-- ucinewgame 
-- position fen <complete-fen> ... moves <square-based move>
-- go depth <n>

-- printf "%s\n" "ucinewgame" "position fen ..." "go depth .." | stockfish
eval :: String -> IO String
eval = P.readProcess "/home/robert/Downloads/stockfish/Linux/stockfish" ["go", "depth", "10"]

-- Stockfish requires moves being written out as square transitions, not