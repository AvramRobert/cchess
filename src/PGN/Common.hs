module PGN.Common where

import Chess.Internal as Chess

hasColour :: Chess.Colour -> Chess.Move -> Bool
hasColour colour = (== colour) . Chess.colour . Chess.position

hasCoord :: Chess.Coord -> Chess.Move -> Bool
hasCoord coord = (== coord) . Chess.coord . Chess.position

hasPiece :: Chess.Piece -> Chess.Move -> Bool
hasPiece piece = (== piece) . Chess.piece . Chess.position

hasX :: Int -> Chess.Move -> Bool
hasX x = (== x) . fst . Chess.coord . Chess.position

hasY :: Int -> Chess.Move -> Bool
hasY y = (== y) . snd . Chess.coord . Chess.position

advancesTo :: Chess.Coord -> Chess.Move -> Bool
advancesTo s (Chess.Advance _ e) = s == e
advancesTo _ _                    = False

capturesAt :: Chess.Coord -> Chess.Move -> Bool
capturesAt s (Chess.Capture _ enemy) = s == (Chess.coord enemy)
capturesAt _ _                       = False

enpassantAt :: Chess.Coord -> Chess.Move -> Bool
enpassantAt s (Chess.Enpassant _ e _) = s == e
enpassantAt _ _                       = False

promotesAs :: Chess.Position -> Chess.Move -> Bool
promotesAs newPos (Chess.Promote _ newPiece enemy) = (Chess.piece newPos) == newPiece && (Chess.coord newPos) == (Chess.coord enemy)
promotesAs _ _                              = False

castlesTowards :: Chess.Dir -> Chess.Move -> Bool
castlesTowards Chess.R (Chess.Castle (_, e) _) = e == (7, 1) || e == (7, 8)
castlesTowards Chess.L (Chess.Castle (_, e) _) = e == (3, 1) || e == (3, 8)
castlesTowards _ _                       = False