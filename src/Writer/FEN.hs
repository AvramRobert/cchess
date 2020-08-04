module Writer.FEN (write) where

import Chess.Internal (Piece (Pawn, Knight, Bishop, Rook, Queen, King, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Castles (None, Both, Long, Short),
                       Position (Pos), Square, Board, Coord, Colour (W, B),
                       coord, movesPiece, past, permitApply, forceApply, emptyBoard, 
                       check, lookAt, coordinates, whiteCastle, blackCastle, player, 
                       halfmoves, fullmoves)
import Data.Char (toLower)
import Data.Maybe (fromJust)
import Control.Monad (mfilter)
import Writer.Common
import Lib.Coll

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
        (Just (Advance (Pos Pawn W (x, 2)) (_, 4))) -> file (x, 2) <> "3"
        (Just (Advance (Pos Pawn B (x, 7)) (_, 5))) -> file (x, 7) <> "6"
        (_)                                         -> "-"

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
fenStats board = (show $ halfmoves board) <> " " <> (show $ fullmoves board)

fenElement :: Position -> Maybe String
fenElement (Pos Empty _ _)  = Nothing
fenElement (Pos Pawn c _)   = Just $ fenNormalise c "P"
fenElement (Pos Bishop c _) = Just $ fenNormalise c "B"
fenElement (Pos Rook c _)   = Just $ fenNormalise c "R"
fenElement (Pos Knight c _) = Just $ fenNormalise c "N"
fenElement (Pos King c _)   = Just $ fenNormalise c "K"
fenElement (Pos Queen c _)  = Just $ fenNormalise c "Q"

write :: Board -> String
write board = (fenBoard board) <> " " <> (fenPlayer board) <> " " <> (fenCastle board) <> " " <> (fenPassant board) <> " " <> (fenStats board)