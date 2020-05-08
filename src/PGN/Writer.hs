module PGN.Writer where

import Chess.Internal (Piece (Pawn, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Position (Pos), Square, Board, 
                       coord, movesPiece, past, forceApply, emptyBoard, check)
import Chess.Display (gameFile, debugRank, standardFigure)
import Lib.Coll
import PGN.Common

movesFor :: Position -> Board -> [Move]
movesFor (Pos p c _) board = movesPiece board (p, c)

label :: Square -> String
label (colour, (x, y)) = gameFile x <> debugRank (colour, (x, y))  

unambigous :: Position -> [Move] -> Bool
unambigous (Pos p c (x, y)) = (== 1) . length

fileUnambigous :: Position -> [Move] -> Bool
fileUnambigous (Pos p c (x, y)) = (== 1) . length . filter (hasX x)

rankUnambigous :: Position -> [Move] -> Bool
rankUnambigous (Pos p c (x, y)) = (== 1) . length . filter (hasY y)

unambigousAdvance :: Move -> String
unambigousAdvance (Advance (Pos Pawn c s) e) = label (c, e)
unambigousAdvance (Advance (Pos p c s) e)    = standardFigure (p, c) <> label (c, e)

fileAmbigousAdvance :: Move -> String
fileAmbigousAdvance (Advance (Pos p c (xs, ys)) e) = standardFigure (p, c) <> gameFile xs <> label (c, e)

rankAmbigousAdvance :: Move -> String
rankAmbigousAdvance (Advance (Pos p c start) e) = standardFigure (p, c) <> debugRank (c, start) <> label (c, e)

totalAdvance :: Move -> String
totalAdvance (Advance (Pos p c start) e) = standardFigure (p, c) <> label (c, start) <> label (c, e)

unambigousCapture :: Move -> String
unambigousCapture (Capture (Pos Pawn c (xs, _)) endp)  = gameFile xs <> "x" <> label (c, coord endp)
unambigousCapture (Capture (Pos p c _)    endp)        = standardFigure (p, c) <> "x" <> label (c, coord endp)

fileAmbigousCapture :: Move -> String
fileAmbigousCapture (Capture (Pos p c (xs, _)) endp) = standardFigure (p, c) <> gameFile xs <> "x" <> label (c, coord endp)

rankAmbigousCapture :: Move -> String
rankAmbigousCapture (Capture (Pos p c start) endp) = standardFigure (p, c) <> debugRank (c, start) <> "x" <> label (c, coord endp)

totalCapture :: Move -> String
totalCapture (Capture (Pos p c start) endp) = standardFigure (p, c) <> label (c, start) <> "x" <> label (c, coord endp)

castle :: Move -> Board -> String
castle (Castle (_, (7, _)) _) _ = "O-O"
castle (Castle (_, (3, _)) _) _ = "O-O-O"

promote :: Move -> Board -> String
promote (Promote (Pos _ c _) np (Pos Empty _ end)) _ = label (c, end) <> "=" <> standardFigure (np, c)  
promote (Promote (Pos p c (xs, _)) _ endp)         _ = gameFile xs <> "x" <> label (c, coord endp) <> "=" <> standardFigure (p, c)  

enpassant :: Move -> Board -> String
enpassant (Enpassant (Pos _ c (xs, _)) end _) _ = gameFile xs <> "x" <> label (c, end)

advance :: Move -> Board -> String
advance move @ (Advance (Pos Pawn _ _) _) board = unambigousAdvance move
advance move @ (Advance pos end) board = let moves = filter (advancesTo end) $ movesFor pos board
                                          in if   (unambigous pos moves)     then unambigousAdvance move
                                          else if (fileUnambigous pos moves) then fileAmbigousAdvance move
                                          else if (rankUnambigous pos moves) then rankAmbigousAdvance move
                                          else                                    totalAdvance move

capture :: Move -> Board -> String
capture move @ (Capture (Pos Pawn _ _) _) board = unambigousCapture move
capture move @ (Capture pos endp)  board = let moves = filter (capturesAt (coord endp)) $ movesFor pos board
                                           in if   (unambigous pos moves)     then unambigousCapture move
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

-- The check is valid after the move was applied, so in order to encode the check properly, I have to use the board after the force apply in order to compute the check
writeApplyMove :: Move -> Board -> (Board, String)
writeApplyMove move board = (board', encodeMove move board <> encodeCheck board')
    where board' = forceApply board move

writeMove :: Move -> Board -> String
writeMove move = snd . writeApplyMove move

writeMoves :: Board -> [String]
writeMoves = map index . zip [1..] . chunksOf 2 . reverse . snd . foldr write (emptyBoard, []) . past
    where write move (board, mvs)   = accumulate mvs $ writeApplyMove move board
          accumulate mvs (board, m) = (board, m : mvs)
          index (i, m1:m2:_)        = show i <> "." <> m1 <> " " <> m2
          index (i, m1:[])          = show i <> "." <> m1 <> " "
          index (i, [])             = ""