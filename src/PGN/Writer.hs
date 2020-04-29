module PGN.Writer (writeMoves) where

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

justOne :: a -> [b] -> Maybe a
justOne a (b:[]) = Just a
justOne _ _      = Nothing

movesFor :: Position -> Board -> [Move]
movesFor (Pos p c _) board = movesPiece board (p, c)

label :: Square -> String
label (colour, (x, y)) = gameFile x <> showRank colour y  

non :: Move -> String
non (Promote (Pos Pawn c _) p endp)  = label (c, coord endp) <> "=" <> standardFigure (p, c)  
non (Advance (Pos Pawn c _) end)     = label (c, end)
non (Advance (Pos p c _)    end)     = standardFigure (p, c) <> label (c, end)
non (Capture (Pos p c _)    endp)    = standardFigure (p, c) <> "x" <> label (c, coord endp)
non (Castle  (_, (7, _)) _)          = "O-O"
non (Castle  (_, (3, _)) _)          = "O-O-O"

file :: Move -> String
file (Enpassant (Pos _ c (xs, _)) end _)  = gameFile xs <> "x" <> label (c, end)
file (Promote (Pos p c (xs, _)) _ endp)   = gameFile xs <> "x" <> label (c, coord endp) <> "=" <> standardFigure (p, c)  
file (Capture (Pos Pawn c (xs, _)) endp)  = gameFile xs <> "x" <> label (c, coord endp)
file (Capture (Pos p c (xs, _)) endp)     = standardFigure (p, c) <> gameFile xs <> "x" <> label (c, coord endp)
file (Advance (Pos p c (xs, _)) end)      = standardFigure (p, c) <> gameFile xs <> label (c, end)

rank :: Move -> String
rank (Capture (Pos p c (_, ys)) endp) = standardFigure (p, c) <> showRank c ys <> "x" <> label (c, coord endp)
rank (Advance (Pos p c (_, ys))  end) = standardFigure (p, c) <> showRank c ys <> label (c, end)

total :: Move -> String
total (Capture (Pos p c start) endp) = standardFigure (p, c) <> label (c, start) <> "x" <> label (c, coord endp)
total (Advance (Pos p c start) end)  = standardFigure (p, c) <> label (c, start) <> label (c, end)

-- I can reuse this (and its counterparts) from Parser 
advancesTo :: Coord -> Move -> Bool
advancesTo c (Advance (Pos _ _ _) e) = c == e
advancesTo _ _                       = False

capturesAt :: Coord -> Move -> Bool
capturesAt s (Capture _ enemy) = s == (coord enemy)
capturesAt _ _                 = False

hasX :: Int -> Move -> Bool
hasX x = (== x) . fst . coord . position

hasY :: Int -> Move -> Bool
hasY y = (== y) . snd . coord . position

encode :: Move -> Ambiguity -> String
encode move Non   = non move
encode move File  = file move
encode move Rank  = rank move
encode move Total = total move

unambigous :: Position -> [Move] -> Maybe Ambiguity
unambigous (Pos p c (x, y)) = justOne Non

fileAmbigous :: Position -> [Move] -> Maybe Ambiguity
fileAmbigous (Pos p c (x, y)) = justOne File . filter (hasX x)

rankAmbigous :: Position -> [Move] -> Maybe Ambiguity
rankAmbigous (Pos p c (x, y)) = justOne Rank . filter (hasY y)

-- You don't need the ADT. You can directly call the transformation functions. When unambigous -> apply unambigous. etc
disambiguate :: Position -> [Move] ->  Ambiguity
disambiguate pos = maybe Total id . mapFirst [unambigous pos, fileAmbigous pos, rankAmbigous pos]

derive :: Board -> Move -> Ambiguity
derive board move = case move of
    (Castle _ _)                  -> Non
    (Promote _ _ (Pos Empty _ _)) -> Non
    (Promote _ _ _)               -> File
    (Advance (Pos Pawn _ _) _)    -> Non
    (Enpassant _ _ _)             -> File
    (Capture (Pos Pawn _ _) _)    -> File
    (Advance pos end)             -> disambiguate pos $ filter (advancesTo end)          $ movesFor pos board
    (Capture pos epos)            -> disambiguate pos $ filter (capturesAt (coord epos)) $ movesFor pos board

writeMoves :: Board -> [String]
writeMoves = snd . foldr write (board, []) . past
    where write move (board, ps) = (forceApply board move, (encode move $ derive board move) : ps)