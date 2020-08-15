module Writer.LAN (write, forceWrite) where

import Chess.Internal (Piece (Pawn, Knight, Bishop, Rook, Queen, King, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Castles (None, Both, Long, Short),
                       Position (Pos), Square, Board, Coord, Colour (W, B),
                       forceApply, permitApply, check)
import Writer.Common
import Lib.Coll

encodeCheck :: Board -> String
encodeCheck board | check board = "+"
encodeCheck board               = ""

encodeMove :: Move -> String
encodeMove (Advance   (Pos p c s) e)                  = piece p <> coord s <> "-" <> coord e
encodeMove (Capture   (Pos p c s) (Pos p' c' e))      = piece p <> coord s <> "x" <> coord e
encodeMove (Enpassant (Pos p c s) e _)                = piece p <> coord s <> "x" <> coord e
encodeMove (Promote   (Pos p c s) p' (Pos Empty _ e)) = piece p <> coord s <> coord e <> "=" <> piece p'
encodeMove (Promote   (Pos p c s) p' (Pos _ _ e))     = piece p <> coord s <> "x" <> coord e <> "=" <> piece p'
encodeMove (Castle (_, (7, _)) _)                     = "O-O"
encodeMove (Castle (_, (3, _)) _)                     = "O-O-O"

forceWriteApply :: Board -> Move -> (Board, String)
forceWriteApply board move = let board' = forceApply board move
                             in (board', encodeMove move <> encodeCheck board')

writeApply :: Board -> Move -> Maybe (Board, String)
writeApply board  move = fmap encoded $ permitApply board move
    where encoded board' = (board', encodeMove move <> encodeCheck board')

write :: Board -> Move -> Maybe String
write board = fmap snd . writeApply board

forceWrite :: Board -> Move -> String
forceWrite move = snd . forceWriteApply move