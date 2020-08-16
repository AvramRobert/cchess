module Writer.Common (piece, file, coord, rank, writeWith) where

import qualified Chess.Internal as Chess
import Lib.Coll (chunksOf)

piece :: Chess.Piece -> String
piece Chess.Pawn   = ""
piece Chess.Rook   = "R"
piece Chess.Knight = "N"
piece Chess.Bishop = "B"
piece Chess.Queen  = "Q"
piece Chess.King   = "K"
piece Chess.Empty  = ""

file :: Chess.Coord -> String
file (1, _) = "a"
file (2, _) = "b"
file (3, _) = "c"
file (4, _) = "d"
file (5, _) = "e"
file (6, _) = "f"
file (7, _) = "g"
file (8, _) = "h"

rank :: Chess.Coord -> String
rank (x, y) = show y

coord :: Chess.Coord -> String
coord c = Writer.Common.file c <> rank c

writeWith :: (Chess.Board -> Chess.Move -> (Chess.Board, String)) -> Chess.Board -> [String]
writeWith writeApply = map index . zip [1..] . chunksOf 2 . reverse . snd . foldr write (Chess.emptyBoard, []) . Chess.past
    where write move (board, mvs)   = accumulate mvs $ writeApply board move 
          accumulate mvs (board, m) = (board, m : mvs)
          index (i, m1:m2:_)        = show i <> "." <> m1 <> " " <> m2
          index (i, m1:[])          = show i <> "." <> m1 <> " "
          index (i, [])             = ""