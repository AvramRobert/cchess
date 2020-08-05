module Writer.LAN (write) where

import Chess.Internal (Piece (Pawn, Knight, Bishop, Rook, Queen, King, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Castles (None, Both, Long, Short),
                       Position (Pos), Square, Board, Coord, Colour (W, B))
import Writer.Common
import Lib.Coll

write :: Move -> String
write (Advance   (Pos p c s) e)                  = piece p <> coord s <> "-" <> coord e
write (Capture   (Pos p c s) (Pos p' c' e))      = piece p <> coord s <> "x" <> coord e
write (Enpassant (Pos p c s) e _)                = piece p <> coord s <> "x" <> coord e
write (Promote   (Pos p c s) p' (Pos Empty _ e)) = piece p <> coord s <> coord e <> "=" <> piece p'
write (Promote   (Pos p c s) p' (Pos _ _ e))     = piece p <> coord s <> "x" <> coord e <> "=" <> piece p'
write (Castle (_, (7, _)) _)                     = "O-O"
write (Castle (_, (3, _)) _)                     = "O-O-O"