module PGN.Writer (writeMoves, writeApplyMove, writeMove) where

import Chess.Internal (Piece (Pawn, Knight, Bishop, Rook, Queen, King, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Position (Pos), Square, Board, Coord, 
                       coord, movesPiece, past, permitApply, forceApply, emptyBoard, check)
import Lib.Coll
import PGN.Common

piece :: Piece -> String
piece Pawn   = "P"
piece Rook   = "R"
piece Knight = "N"
piece Bishop = "B"
piece Queen  = "Q"
piece King   = "K"
piece Empty  = ""

rank :: Coord -> String
rank (x, y) = show y

file :: Coord -> String
file (1, _) = "a"
file (2, _) = "b"
file (3, _) = "c"
file (4, _) = "d"
file (5, _) = "e"
file (6, _) = "f"
file (7, _) = "g"
file (8, _) = "h"

movesFor :: Position -> Board -> [Move]
movesFor (Pos p c _) board = movesPiece board (p, c)

label :: Coord -> String
label coord = file coord <> rank coord  

unambigous :: Position -> [Move] -> Bool
unambigous (Pos p c (x, y)) = (== 1) . length

fileUnambigous :: Position -> [Move] -> Bool
fileUnambigous (Pos p c (x, y)) = (== 1) . length . filter (hasX x)

rankUnambigous :: Position -> [Move] -> Bool
rankUnambigous (Pos p c (x, y)) = (== 1) . length . filter (hasY y)

unambigousAdvance :: Move -> String
unambigousAdvance (Advance (Pos Pawn c s) end) = label end
unambigousAdvance (Advance (Pos p c s) end)    = piece p <> label end

fileAmbigousAdvance :: Move -> String
fileAmbigousAdvance (Advance (Pos p c start) end) = piece p <> file start <> label end

rankAmbigousAdvance :: Move -> String
rankAmbigousAdvance (Advance (Pos p c start) end) = piece p <> rank start <> label end

totalAdvance :: Move -> String
totalAdvance (Advance (Pos p c start) end) = piece p <> label start <> label end

unambigousCapture :: Move -> String
unambigousCapture (Capture (Pos Pawn c start) endp) = file start <> "x" <> label (coord endp)
unambigousCapture (Capture (Pos p c _)    endp)     = piece p    <> "x" <> label (coord endp)

fileAmbigousCapture :: Move -> String
fileAmbigousCapture (Capture (Pos p c start) endp) = piece p <> file start <> "x" <> label (coord endp)

rankAmbigousCapture :: Move -> String
rankAmbigousCapture (Capture (Pos p c start) endp) = piece p <> rank start <> "x" <> label (coord endp)

totalCapture :: Move -> String
totalCapture (Capture (Pos p c start) endp) = piece p <> label start <> "x" <> label (coord endp)

castle :: Move -> Board -> String
castle (Castle (_, (7, _)) _) _ = "O-O"
castle (Castle (_, (3, _)) _) _ = "O-O-O"

promote :: Move -> Board -> String
promote (Promote (Pos _ c _) np (Pos Empty _ end)) _ = label end  <> "=" <> piece np  
promote (Promote (Pos _ c start) np endp)          _ = file start <> "x" <> label (coord endp) <> "=" <> piece np  

enpassant :: Move -> Board -> String
enpassant (Enpassant (Pos _ c start) end _) _ = file start <> "x" <> label end

advance :: Move -> Board -> String
advance move @ (Advance (Pos Pawn _ _) _) board = unambigousAdvance move
advance move @ (Advance pos end) board = let moves = filter (advancesTo end) $ movesFor pos board
                                          in   if (unambigous pos moves)     then unambigousAdvance move
                                          else if (fileUnambigous pos moves) then fileAmbigousAdvance move
                                          else if (rankUnambigous pos moves) then rankAmbigousAdvance move
                                          else                                    totalAdvance move

capture :: Move -> Board -> String
capture move @ (Capture (Pos Pawn _ _) _) board = unambigousCapture move
capture move @ (Capture pos endp)  board = let moves = filter (capturesAt (coord endp)) $ movesFor pos board
                                           in   if (unambigous pos moves)     then unambigousCapture move
                                           else if (fileUnambigous pos moves) then fileAmbigousCapture move
                                           else if (rankUnambigous pos moves) then rankAmbigousCapture move
                                           else                                    totalCapture move

encodeMove :: Move -> Board ->  String
encodeMove move @ (Castle _ _)       = castle move
encodeMove move @ (Promote _ _ _)    = promote move
encodeMove move @ (Enpassant _ _ _)  = enpassant move 
encodeMove move @ (Advance _ _)      = advance move
encodeMove move @ (Capture _ _)      = capture move

encodeCheck :: Board -> String
encodeCheck board | check board = "+"
encodeCheck board               = ""

forceWriteApplyMove :: Move -> Board -> (Board, String)
forceWriteApplyMove move board = encode $ forceApply board move
    where encode board' = (board', encodeMove move board <> encodeCheck board')

-- The check is valid after the move was applied, so in order to encode the check properly, I have to use the board after the force apply in order to compute the check
writeApplyMove :: Move -> Board -> Maybe (Board, String)
writeApplyMove move board = fmap encode $ permitApply board move
    where encode board' = (board', encodeMove move board <> encodeCheck board')

writeMove :: Move -> Board -> Maybe String
writeMove move = fmap snd . writeApplyMove move

writeMoves :: Board -> [String]
writeMoves = map index . zip [1..] . chunksOf 2 . reverse . snd . foldr write (emptyBoard, []) . past
    where write move (board, mvs)   = accumulate mvs $ forceWriteApplyMove move board
          accumulate mvs (board, m) = (board, m : mvs)
          index (i, m1:m2:_)        = show i <> "." <> m1 <> " " <> m2
          index (i, m1:[])          = show i <> "." <> m1 <> " "
          index (i, [])             = ""