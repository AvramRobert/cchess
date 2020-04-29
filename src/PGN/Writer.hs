module PGN.Writer where

import Chess.Internal (Piece (King, Queen, Rook, Bishop, Knight, Pawn, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Colour(W, B), Position (Pos),
                       Castles (Both, Long, Short, None), 
                       Square, Figure, Coord, Board, 
                       player, coord, figure, colour, movesPiece, past, forceApply, board, piece, position)
import Control.Applicative ((<|>))
import Chess.Display (gameFile, showRank, standardFigure)
import Data.List (find)
import Data.Maybe (isJust)
import Lib.Coll

data Ambiguity = Non | File | Rank | Total

movesFor :: Position -> Board -> [Move]
movesFor (Pos p c _) board = movesPiece board (p, c)

label :: Square -> String
label (colour, (x, y)) = gameFile x <> showRank colour y  

non :: Move -> String
non (Advance (Pos Pawn c _) end)                  = label (c, end)
non (Advance (Pos p c _)    end)                  = standardFigure (p, c) <> label (c, end)
non (Promote (Pos Pawn c _) p (Pos Empty _ end))  = label (c, end) <> "=" <> standardFigure (p, c)  
non (Promote (Pos Pawn c (x, _)) p (Pos _ _ end)) = gameFile x <> "x" <> label (c, end) <> "=" <> standardFigure (p, c)  

file :: Move -> String
file (Capture (Pos Pawn c (xs, _)) endp) = gameFile xs <> "x" <> label (c, coord endp)

-- I can reuse this (and its counterparts) from Parser 
advancesTo :: Coord -> Move -> Bool
advancesTo c (Advance (Pos _ _ _) e) = c == e
advancesTo _ _                       = False

capturesAt :: Coord -> Move -> Bool
capturesAt s (Capture _ enemy) = s == (coord enemy)
capturesAt s (Enpassant _ e _) = s == e
capturesAt _ _                 = False

encode :: Move -> Ambiguity -> String
encode move Non = non move

-- ambiguity -> there's more than one piece of that type that can go to (xe, ye)

-- 1. unambigous: there's only one piece of that type that can go (xe, ye)
-- 2. fileambigous: there's only one piece of that type with that ox that can go to (xe, ye)
-- 3. rankambigous: there's only one piece of that type with that oy that can go to (xe, ye)
-- 4. explicit: (ox, oy) to (xe, ye)

justOne :: a -> [b] -> Maybe a
justOne a (b:[]) = Just a
justOne _ _      = Nothing

unambigous :: Position -> [Move] -> Maybe Ambiguity
unambigous (Pos p c _) = justOne Non

fileAmbigous :: Position -> [Move] -> Maybe Ambiguity
fileAmbigous (Pos p c _) = justOne File -- fix this

rankAmbigous :: Position -> [Move] -> Maybe Ambiguity
rankAmbigous (Pos p c _) = justOne Rank

disambiguate :: Position -> [Move] ->  Ambiguity
disambiguate pos = maybe Total id . mapFirst [unambigous pos, fileAmbigous pos, rankAmbigous pos]

derive :: Board -> Move -> Ambiguity
derive board move = case move of
    (Promote _ _ _)             -> Non
    (Castle _ _)                -> Non
    (Enpassant _ _ _)           -> Non
    (Advance (Pos Pawn _ _) _)  -> Non
    (Advance pos end)           -> disambiguate pos $ filter (advancesTo end)          $ movesFor pos board
    (Capture pos epos)          -> disambiguate pos $ filter (capturesAt (coord epos)) $ movesFor pos board

write :: Board -> [String]
write = snd . foldr write (board, []) . past
    where write move (board, ps) = (forceApply board move, (encode move $ derive board move) : ps)
                   
