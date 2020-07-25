module PGN.Writer (writeMoves, writeApplyMove, writeMove, fen, fenBoard) where

import Chess.Internal (Piece (Pawn, Knight, Bishop, Rook, Queen, King, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Castles (None, Both, Long, Short),
                       Position (Pos), Square, Board, Coord, Colour (W, B),
                       coord, movesPiece, past, permitApply, forceApply, emptyBoard, 
                       check, lookAt, coordinates, whiteCastle, blackCastle, player, 
                       halfmoves, totalmoves)
import Lib.Coll
import PGN.Common
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Control.Monad (mfilter)

rows = 
    [
     [(1,1), (2,1), (3,1), (4,1), (5,1), (6,1), (7,1), (8,1)],
     [(1,2), (2,2), (3,2), (4,2), (5,2), (6,2), (7,2), (8,2)],
     [(1,3), (2,3), (3,3), (4,3), (5,3), (6,3), (7,3), (8,3)],
     [(1,4), (2,4), (3,4), (4,4), (5,4), (6,4), (7,4), (8,4)],
     [(1,5), (2,5), (3,5), (4,5), (5,5), (6,5), (7,5), (8,5)],
     [(1,6), (2,6), (3,6), (4,6), (5,6), (6,6), (7,6), (8,6)],
     [(1,7), (2,7), (3,7), (4,7), (5,7), (6,7), (7,7), (8,7)],
     [(1,8), (2,8), (3,8), (4,8), (5,8), (6,8), (7,8), (8,8)]]

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

fenNormalise :: Colour -> String -> String
fenNormalise W = id
fenNormalise B = map toLower

fenCastle :: Board -> String
fenCastle board = case (whiteCastle board, blackCastle board) of
        (None, None)   -> "-"
        (white, black) -> castle W white <> castle B black
    where castle c Both  = fenNormalise c "KG"
          castle c Short = fenNormalise c "K"
          castle c Long  = fenNormalise c "Q" 

fenPassant :: Board -> String
fenPassant board = case (first $ past board) of
        (Just (Advance (Pos Pawn W (x, 2)) (_, 4))) -> file x <> "3"
        (Just (Advance (Pos Pawn B (x, 7)) (_, 5))) -> file x <> "6"
        (_)                                         -> "-"
    where file 1 = "a"
          file 2 = "b"
          file 3 = "c"
          file 4 = "d"
          file 5 = "e"
          file 6 = "f"
          file 7 = "g"
          file 8 = "h"

fenPlayer :: Board -> String
fenPlayer board = case (player board) of W -> "w"
                                         B -> "b"

fenBoard :: Board -> String
fenBoard board = tail $ foldr (\r s -> s <> "/" <> track r 0 "") "" rows
    where track [] n e     = write n e
          track (x:xs) n e = maybe (track xs (n + 1) e) (\en -> track xs 0 $ (write n e <> en)) $ fenElement $ fromJust $ lookAt board x
          write 0 e        = e
          write n e        = e <> show n

fenStats :: Board -> String
fenStats board = (show $ halfmoves board) <> " " <> (show $ totalmoves board)

fenElement :: Position -> Maybe String
fenElement (Pos Empty _ _)  = Nothing
fenElement (Pos Pawn c _)   = Just $ fenNormalise c "P"
fenElement (Pos Bishop c _) = Just $ fenNormalise c "B"
fenElement (Pos Rook c _)   = Just $ fenNormalise c "R"
fenElement (Pos Knight c _) = Just $ fenNormalise c "N"
fenElement (Pos King c _)   = Just $ fenNormalise c "K"
fenElement (Pos Queen c _)  = Just $ fenNormalise c "Q"

fen :: Board -> String
fen board = (fenBoard board) <> " " <> (fenPlayer board) <> " " <> (fenCastle board) <> " " <> (fenPassant board) <> " " <> (fenStats board)