module PGN.Writer where

import Chess.Internal (Piece (King, Queen, Rook, Bishop, Knight, Pawn, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Colour(W, B), Position (Pos),
                       Castles (Both, Long, Short, None), 
                       Square, Figure, Coord, Board, 
                       player, coord, figure, colour, movesPiece, past, 
                       forceApply, emptyBoard, piece, position, check)
import Control.Applicative ((<|>))
import Chess.Display (gameFile, debugRank, standardFigure)
import Data.List (find)
import Data.Maybe (isJust)
import Lib.Coll

justOne :: a -> [b] -> Maybe a
justOne a (b:[]) = Just a
justOne _ _      = Nothing

movesFor :: Position -> Board -> [Move]
movesFor (Pos p c _) board = movesPiece board (p, c)

label :: Square -> String
label (colour, (x, y)) = gameFile x <> debugRank (colour, (x, y))  

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
rank (Capture (Pos p c start) endp) = standardFigure (p, c) <> debugRank (c, start) <> "x" <> label (c, coord endp)
rank (Advance (Pos p c start)  end) = standardFigure (p, c) <> debugRank (c, start) <> label (c, end)

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

unambigous :: Move -> Position -> [Move] -> Maybe String
unambigous move (Pos p c (x, y)) = justOne (non move)

fileAmbigous :: Move -> Position -> [Move] -> Maybe String
fileAmbigous move (Pos p c (x, y)) = justOne (file move) . filter (hasX x)

rankAmbigous :: Move -> Position -> [Move] -> Maybe String
rankAmbigous move (Pos p c (x, y)) = justOne (rank move) . filter (hasY y)

disambiguate :: Move -> Position -> [Move] -> String
disambiguate move pos = maybe (total move) id . mapFirst [unambigous move pos, fileAmbigous move pos, rankAmbigous move pos]

encodeMove :: Board -> Move -> String
encodeMove board move = serialise move
    where serialise (Castle _ _)                  = non move
          serialise (Promote _ _ (Pos Empty _ _)) = non move
          serialise (Promote _ _ _)               = file move
          serialise (Advance (Pos Pawn _ _) _)    = non move
          serialise (Enpassant _ _ _)             = file move
          serialise (Capture (Pos Pawn _ _) _)    = file move
          serialise (Advance pos end)             = disambiguate move pos $ filter (advancesTo end)          $ movesFor pos board
          serialise (Capture pos epos)            = disambiguate move pos $ filter (capturesAt (coord epos)) $ movesFor pos board

encodeCheck :: Board -> String
encodeCheck board | check board = "+"
encodeCheck board               = ""

-- The check is valid after the move was applied, so in order to encode the check properly, I have to use the board after the force apply in order to compute the check
writeMoves :: Board -> [String]
writeMoves = map index . zip [1..] . chunksOf 2 . reverse . snd . foldr write (emptyBoard, []) . past
    where write move (board, mvs) = let board' = forceApply board move 
                                        emove  = encodeMove board move
                                        echeck = encodeCheck board'
                                    in (board', (emove <> echeck) : mvs) 
          index (i, m1:m2:_)      = show i <> "." <> m1 <> " " <> m2
          index (i, m1:[])        = show i <> "." <> m1 <> " "
          index (i, [])           = ""