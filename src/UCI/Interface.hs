module UCI.Interface where

import qualified System.Process as P
import qualified GHC.IO.Handle as H
import qualified Chess.Game as G
import qualified Chess.Internal as I
import qualified Writer.LAN as LAN
import qualified Writer.FEN as FEN
import qualified Writer.PGN as PGN

stockfish = "/home/robert/Downloads/stockfish/Linux/stockfish" 

position :: G.Game -> I.Move -> String
position game move = "position fen " <> FEN.write (G.gameBoard game) <> " moves " <> LAN.forceWrite (G.gameBoard game) move 

newgame :: String
newgame = "ucinewgame"

calculate :: Int -> String
calculate depth = "go depth " <> show depth

enter :: String -> String
enter s = s <> "\n"

eval :: Int -> G.Game -> I.Move -> IO String
eval depth game move = do 
    (Just input, Just output, _, ph) <- P.createProcess (P.proc stockfish []) { P.std_out = P.CreatePipe, P.std_in = P.CreatePipe }
    H.hSetBuffering output H.NoBuffering
    H.hPutStr input (enter $ newgame)
    H.hPutStr input (enter $ position game move)
    H.hPutStr input (enter $ calculate depth)
    H.hGetLine output
    H.hGetLine output