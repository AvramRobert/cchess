module Writer.PGN (write, writeApply, writeFor) where

import Chess.Internal (Piece (Pawn, Knight, Bishop, Rook, Queen, King, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Castles (None, Both, Long, Short),
                       Position (Pos), Square, Board, Coord, Colour (W, B),
                       movesPiece, past, permitApply, forceApply, emptyBoard, 
                       check, lookAt, coordinates, whiteCastle, blackCastle, player, 
                       halfmoves, fullmoves)
import Parser.Common (advancesTo, capturesAt, hasY, hasX)
import Writer.Common
import Lib.Coll

movesFor :: Position -> Board -> [Move]
movesFor (Pos p c _) board = movesPiece board (p, c)

unambigous :: Position -> [Move] -> Bool
unambigous (Pos p c (x, y)) = (== 1) . length

fileUnambigous :: Position -> [Move] -> Bool
fileUnambigous (Pos p c (x, y)) = (== 1) . length . filter (hasX x)

rankUnambigous :: Position -> [Move] -> Bool
rankUnambigous (Pos p c (x, y)) = (== 1) . length . filter (hasY y)

unambigousAdvance :: Move -> String
unambigousAdvance (Advance (Pos Pawn c s) e) = coord e
unambigousAdvance (Advance (Pos p c s) e)    = piece p <> coord e

fileAmbigousAdvance :: Move -> String
fileAmbigousAdvance (Advance (Pos p c s) e) = piece p <> file s <> coord e

rankAmbigousAdvance :: Move -> String
rankAmbigousAdvance (Advance (Pos p c s) e) = piece p <> rank s <> coord e

totalAdvance :: Move -> String
totalAdvance (Advance (Pos p c s) e) = piece p <> coord s <> coord e

unambigousCapture :: Move -> String
unambigousCapture (Capture (Pos Pawn c s) (Pos _ _ e)) = file s <> "x" <> coord e
unambigousCapture (Capture (Pos p c _) (Pos _ _ e))    = piece p <> "x" <> coord e

fileAmbigousCapture :: Move -> String
fileAmbigousCapture (Capture (Pos p c s) (Pos _ _ e)) = piece p <> file s <> "x" <> coord e

rankAmbigousCapture :: Move -> String
rankAmbigousCapture (Capture (Pos p c s) (Pos _ _ e)) = piece p <> rank s <> "x" <> coord e

totalCapture :: Move -> String
totalCapture (Capture (Pos p c s) (Pos _ _ e)) = piece p <> coord s <> "x" <> coord e

castle :: Move -> Board -> String
castle (Castle (_, (7, _)) _) _ = "O-O"
castle (Castle (_, (3, _)) _) _ = "O-O-O"

promote :: Move -> Board -> String
promote (Promote (Pos _ c _) np (Pos Empty _ e)) _ = coord e  <> "=" <> piece np  
promote (Promote (Pos _ c s) np (Pos _ _ e))     _ = file s <> "x" <> coord e <> "=" <> piece np  

enpassant :: Move -> Board -> String
enpassant (Enpassant (Pos _ c s) e _) _ = file s <> "x" <> coord e

advance :: Move -> Board -> String
advance move @ (Advance (Pos Pawn _ _) _) board = unambigousAdvance move
advance move @ (Advance pos end) board = let moves = filter (advancesTo end) $ movesFor pos board
                                          in   if (unambigous pos moves)     then unambigousAdvance move
                                          else if (fileUnambigous pos moves) then fileAmbigousAdvance move
                                          else if (rankUnambigous pos moves) then rankAmbigousAdvance move
                                          else                                    totalAdvance move

capture :: Move -> Board -> String
capture move @ (Capture (Pos Pawn _ _) _)  board = unambigousCapture move
capture move @ (Capture pos (Pos _ _ end)) board = let moves = filter (capturesAt end) $ movesFor pos board
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

forceWriteApply :: Board -> Move -> (Board, String)
forceWriteApply board move = let board' = forceApply board move
                             in (board', encodeMove move board <> encodeCheck board')

-- The check is visible after the move was applied. 
-- In order to encode it properly, I have to use the board after the force apply
writeApply :: Board -> Move -> Maybe (Board, String)
writeApply board move = fmap encode $ permitApply board move
    where encode board' = (board', encodeMove move board <> encodeCheck board')

write :: Board -> Move -> Maybe String
write board = fmap snd . writeApply board

writeFor :: Board -> [String]
writeFor = map index . zip [1..] . chunksOf 2 . reverse . snd . foldr write (emptyBoard, []) . past
    where write move (board, mvs)   = accumulate mvs $ forceWriteApply board move 
          accumulate mvs (board, m) = (board, m : mvs)
          index (i, m1:m2:_)        = show i <> "." <> m1 <> " " <> m2
          index (i, m1:[])          = show i <> "." <> m1 <> " "
          index (i, [])             = ""