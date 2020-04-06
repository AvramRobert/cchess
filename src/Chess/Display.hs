module Chess.Display where

import Chess.Internal (Piece (King, Queen, Rook, Bishop, Knight, Pawn, Empty), 
                       Colour(W, B), Position (Pos), 
                       Square, Figure, Coord, Board, lookAt, figure)
import Data.Maybe (maybe)
import Data.List (maximumBy, intersperse)
import Lib.Coll

data DisplayMode = GameMode | DebugMode deriving (Eq, Show)

boardCoord :: Square -> Coord
boardCoord (W, s)      = s
boardCoord (B, (x, y)) = (x, 9 - y) 

gameFigure :: Figure -> String
gameFigure (Pawn, W)   = "♙"
gameFigure (Pawn, B)   = "♟"
gameFigure (Bishop, W) = "♗"
gameFigure (Bishop, B) = "♝"
gameFigure (King, W)   = "♔"
gameFigure (King, B)   = "♚"
gameFigure (Rook, W)   = "♖"
gameFigure (Rook, B)   = "♜"
gameFigure (Knight, W) = "♘"
gameFigure (Knight, B) = "♞"
gameFigure (Queen, W)  = "♕"
gameFigure (Queen, B)  = "♛"
gameFigure (Empty, _)  = "-"

gameLabel :: Square -> String
gameLabel (_, (1, _)) = "A"
gameLabel (_, (2, _)) = "B"
gameLabel (_, (3, _)) = "C"
gameLabel (_, (4, _)) = "D"
gameLabel (_, (5, _)) = "E"
gameLabel (_, (6, _)) = "F"
gameLabel (_, (7, _)) = "G"
gameLabel (_, (8, _)) = "H"

debugLabel :: Square -> String
debugLabel = show . fst . snd

debugFigure :: Figure -> String
debugFigure (Pawn, W)   = "Pawn (W)"
debugFigure (Pawn, B)   = "Pawn (B)"
debugFigure (Bishop, W) = "Bishop (W)"
debugFigure (Bishop, B) = "Bishop (B)"
debugFigure (King, W)   = "King (W)"
debugFigure (King, B)   = "King (B)"
debugFigure (Rook, W)   = "Rook (W)"
debugFigure (Rook, B)   = "Rook (B)"
debugFigure (Knight, W) = "Knight (W)"
debugFigure (Knight, B) = "Knight (B)"
debugFigure (Queen, W)  = "Queen (W)"
debugFigure (Queen, B)  = "Queen (B)"
debugFigure (Empty, _)  = "-" 

showLabel :: DisplayMode -> Square -> String
showLabel mode square = case mode of GameMode  -> gameLabel square
                                     DebugMode -> debugLabel square

showIndex :: Square -> String
showIndex = show. snd . boardCoord

showPiece :: DisplayMode -> Board -> Square -> String
showPiece mode board square = case mode of GameMode  -> display gameFigure
                                           DebugMode -> display debugFigure 
        where display f = maybe (show Empty) (f . figure) $ lookAt board $ boardCoord square


-- it would be nice to have a display function that has variable width
-- something where I can display file by file, provide all strings that go in that file and it picks the one with the largest width, adds padding and creates the file


-- find the largest string
-- pad every the smaller ones with as much as necessary to get to its size
-- pad left and right with 1

string :: [String] -> String
string = foldr (<>) ""

topPad :: String -> String
topPad = string . map (const "‾")

bottomPad :: String -> String
bottomPad = string . map (const " ")

padEntry :: [String] -> [String]
padEntry items = fmap (pad . embroid) items
    where embroid s = s <> padding (largest - length s)              
          largest   = length $ maximumBy size items
          pad s     = "  " <> s <> "  "
          padding n = string $ take n $ repeat " "
          size a b  = let la = length a 
                          lb = length b
                      in if (la > lb) then GT
                         else if (la < lb) then LT
                         else EQ

columned :: [String] -> [String]
columned = foldr merge [] . padEntry
    where merge entry xs = case (cell entry) of (t:m:b:[]) -> t : m : b : xs
          cell s     = [topPad s, s, bottomPad s]

rowed :: [[String]] -> [[String]]
rowed entries = let columns = fmap columned entries
                    amount  = length (head columns) * length (head entries) 
                    rows    = take amount $ repeat []
                in foldr (zipWith (:)) rows columns

grid :: [[String]] -> String
grid = unlines . bottomOut . fmap (enclose . foldr (<>) "|" . intersperse "|") . rowed
    where enclose s = "|" <> s
          bottomOut xs = xs <> [(topPad $ head xs)] 


boardFor' :: DisplayMode -> Board -> Colour -> String
boardFor' mode board colour = grid $ chunksOf 8 pieces 
    where p s = showPiece mode board (colour, s)
          pieces =  do x <- [8,7..1] -- because we foldr
                       y <- [8,7..1] -- because we foldr
                       return $ p (x, y)

boardFor :: Board -> Colour -> String
boardFor board colour = unlines $ 
    [" "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "    ",
     " "      <>     "      "<> l (1, 8) <>"     "<> l (2, 8) <>"     "<> l (3, 8) <>"     "<> l (4, 8) <>"     "<> l (5, 8) <>"     "<> l (6, 8) <>"     "<> l (7, 8) <>"     "<> l (8, 8) <>"   ",
     " "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "    ",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<> i (1, 8) <>"  |  "<> p (1, 8) <>"  |  "<> p (2, 8) <>"  |  "<> p (3, 8) <>"  |  "<> p (4, 8) <>"  |  "<> p (5, 8) <>"  |  "<> p (6, 8) <>"  |  "<> p (7, 8) <>"  |  "<> p (8, 8) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<> i (1, 7) <>"  |  "<> p (1, 7) <>"  |  "<> p (2, 7) <>"  |  "<> p (3, 7) <>"  |  "<> p (4, 7) <>"  |  "<> p (5, 7) <>"  |  "<> p (6, 7) <>"  |  "<> p (7, 7) <>"  |  "<> p (8, 7) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<> i (1, 6) <>"  |  "<> p (1, 6) <>"  |  "<> p (2, 6) <>"  |  "<> p (3, 6) <>"  |  "<> p (4, 6) <>"  |  "<> p (5, 6) <>"  |  "<> p (6, 6) <>"  |  "<> p (7, 6) <>"  |  "<> p (8, 6) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<> i (1, 5) <>"  |  "<> p (1, 5) <>"  |  "<> p (2, 5) <>"  |  "<> p (3, 5) <>"  |  "<> p (4, 5) <>"  |  "<> p (5, 5) <>"  |  "<> p (6, 5) <>"  |  "<> p (7, 5) <>"  |  "<> p (8, 5) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<> i (1, 4) <>"  |  "<> p (1, 4) <>"  |  "<> p (2, 4) <>"  |  "<> p (3, 4) <>"  |  "<> p (4, 4) <>"  |  "<> p (5, 4) <>"  |  "<> p (6, 4) <>"  |  "<> p (7, 4) <>"  |  "<> p (8, 4) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<> i (1, 3) <>"  |  "<> p (1, 3) <>"  |  "<> p (2, 3) <>"  |  "<> p (3, 3) <>"  |  "<> p (4, 3) <>"  |  "<> p (5, 3) <>"  |  "<> p (6, 3) <>"  |  "<> p (7, 3) <>"  |  "<> p (8, 3) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<> i (1, 2) <>"  |  "<> p (1, 2) <>"  |  "<> p (2, 2) <>"  |  "<> p (3, 2) <>"  |  "<> p (4, 2) <>"  |  "<> p (5, 2) <>"  |  "<> p (6, 2) <>"  |  "<> p (7, 2) <>"  |  "<> p (8, 2) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<> i (1, 1) <>"  |  "<> p (1, 1) <>"  |  "<> p (2, 1) <>"  |  "<> p (3, 1) <>"  |  "<> p (4, 1) <>"  |  "<> p (5, 1) <>"  |  "<> p (6, 1) <>"  |  "<> p (7, 1) <>"  |  "<> p (8, 1) <>"  |",
     " "      <>     "   |__"      <>     "___|__"      <>     "___|__"      <>     "___|__"      <>     "___|__"      <>     "___|__"      <>     "___|__"      <>     "___|__"      <>     "___|",
     " "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "    ",
     " "      <>     "      "<> l (1, 8) <>"     "<> l (2, 8) <>"     "<> l (3, 8) <>"     "<> l (4, 8) <>"     "<> l (5, 8) <>"     "<> l (6, 8) <>"     "<> l (7, 8) <>"     "<> l (8, 8) <>"   ",
     " "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "    "]
     where p s = showPiece GameMode board (colour, s)
           l s = showLabel GameMode (colour, s)
           i s = showIndex (colour, s)


template :: Board -> Colour -> String
template board colour = unlines $ 
    [a <> s <> p <> p <> p <> p <> p <> p <> p <> p <> p <> p <> p <> p <> p <> p,
     a <> s <> l <> s <> l <> s <> l <> s <> l <> s <> l <> s <> l <> s <> l <> s,
     a <> s <> p <> s <> p <> s <> p <> s <> p <> s <> p <> s <> p <> s <> p <> s,
     
     a <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m,
     i <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m,
     a <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m,

     a <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m,
     i <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m,
     a <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m,
     
     a <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m,
     i <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m,
     a <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m,
     
     a <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m,
     i <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m,
     a <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m,
     
     a <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m,
     i <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m,
     a <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m,
     
     a <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m,
     i <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m,
     a <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m,
     
     a <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m <> t <> m,
     i <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m <> e <> m,
     a <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m <> b <> m,
     
     a <> s <> t <> s <> t <> s <> t <> s <> t <> s <> t <> s <> t <> s <> t <> s,
     a <> s <> l <> s <> l <> s <> l <> s <> l <> s <> l <> s <> l <> s <> l <> s]
    where e = " P " -- piece entry
          t = string $ map (const "‾") e -- top line => dependent on entry size
          b = string $ map (const " ") e -- bottom pad => dependent on entry size
          i = " 1  " -- index entry
          a = string $ map (const " ") i -- index pad => dependent on index entry size
          m = "|" -- delimiter entry
          s = string $ map (const " ") m -- stub pad => dependent on delimiter entry size
          l = " A " -- label entry => should actually pe put in the moddile of a list, whose size is dependent on `entry`
          p = string $ map (const " ") l -- label pad => dependent on the label size