module Writer.Common (piece, file, coord, rank) where

import qualified Chess.Internal as Chess

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