module Chess.Display where

import Chess.Internal (Piece (King, Queen, Rook, Bishop, Knight, Pawn, Empty), 
                       Colour(W, B), Position (Pos), 
                       Square, Figure, Coord, Board, lookAt, figure, coordinates)
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

gameLabel :: Int -> String
gameLabel 1 = "A"
gameLabel 2 = "B"
gameLabel 3 = "C"
gameLabel 4 = "D"
gameLabel 5 = "E"
gameLabel 6 = "F"
gameLabel 7 = "G"
gameLabel 8 = "H"

debugLabel :: Int -> String
debugLabel = show

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

showLabel :: DisplayMode -> Int -> String
showLabel mode x = case mode of GameMode  -> gameLabel x
                                DebugMode -> debugLabel x

showIndex :: Colour -> Int -> String
showIndex c i = show $ snd $ boardCoord $ (c, (1, i))

showPiece :: DisplayMode -> Figure -> String
showPiece GameMode  = gameFigure
showPiece DebugMode = debugFigure

showPosition :: DisplayMode -> Board -> Square -> String
showPosition mode board square = maybe (show Empty) (showPiece mode . figure) $ lookAt board $ boardCoord square


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
    where p s = showPosition mode board (colour, s)
          pieces =  do x <- [8,7..1] -- because we foldr
                       y <- [8,7..1] -- because we foldr
                       return $ p (x, y)

boardFor :: Board -> Colour -> String
boardFor board colour = unlines $ 
    [" "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "    ",
     " "      <>     "      "<>   l 1   <>"     "<>    l 2   <>"     "<>    l 3   <>"     "<>    l 4   <>"     "<>    l 5   <>"     "<>    l 6   <>"     "<>    l 7   <>"     "<>    l 8   <>"   ",
     " "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "    ",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<>    i 8   <>"  |  "<> p (1, 8) <>"  |  "<> p (2, 8) <>"  |  "<> p (3, 8) <>"  |  "<> p (4, 8) <>"  |  "<> p (5, 8) <>"  |  "<> p (6, 8) <>"  |  "<> p (7, 8) <>"  |  "<> p (8, 8) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<>    i 7  <>"  |  "<> p (1, 7) <>"  |  "<> p (2, 7) <>"  |  "<> p (3, 7) <>"  |  "<> p (4, 7) <>"  |  "<> p (5, 7) <>"  |  "<> p (6, 7) <>"  |  "<> p (7, 7) <>"  |  "<> p (8, 7) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<>    i 6  <>"  |  "<> p (1, 6) <>"  |  "<> p (2, 6) <>"  |  "<> p (3, 6) <>"  |  "<> p (4, 6) <>"  |  "<> p (5, 6) <>"  |  "<> p (6, 6) <>"  |  "<> p (7, 6) <>"  |  "<> p (8, 6) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<>    i 5  <>"  |  "<> p (1, 5) <>"  |  "<> p (2, 5) <>"  |  "<> p (3, 5) <>"  |  "<> p (4, 5) <>"  |  "<> p (5, 5) <>"  |  "<> p (6, 5) <>"  |  "<> p (7, 5) <>"  |  "<> p (8, 5) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<>    i 4  <>"  |  "<> p (1, 4) <>"  |  "<> p (2, 4) <>"  |  "<> p (3, 4) <>"  |  "<> p (4, 4) <>"  |  "<> p (5, 4) <>"  |  "<> p (6, 4) <>"  |  "<> p (7, 4) <>"  |  "<> p (8, 4) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<>    i 3  <>"  |  "<> p (1, 3) <>"  |  "<> p (2, 3) <>"  |  "<> p (3, 3) <>"  |  "<> p (4, 3) <>"  |  "<> p (5, 3) <>"  |  "<> p (6, 3) <>"  |  "<> p (7, 3) <>"  |  "<> p (8, 3) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<>    i 2  <>"  |  "<> p (1, 2) <>"  |  "<> p (2, 2) <>"  |  "<> p (3, 2) <>"  |  "<> p (4, 2) <>"  |  "<> p (5, 2) <>"  |  "<> p (6, 2) <>"  |  "<> p (7, 2) <>"  |  "<> p (8, 2) <>"  |",
     " "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |  "      <>     "   |",
     " "      <>     "   |‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|‾‾"      <>     "‾‾‾|",
     " "<>    i 1  <>"  |  "<> p (1, 1) <>"  |  "<> p (2, 1) <>"  |  "<> p (3, 1) <>"  |  "<> p (4, 1) <>"  |  "<> p (5, 1) <>"  |  "<> p (6, 1) <>"  |  "<> p (7, 1) <>"  |  "<> p (8, 1) <>"  |",
     " "      <>     "   |__"      <>     "___|__"      <>     "___|__"      <>     "___|__"      <>     "___|__"      <>     "___|__"      <>     "___|__"      <>     "___|__"      <>     "___|",
     " "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "    ",
     " "      <>     "      "<>   l 1   <>"     "<>    l 2   <>"     "<>    l 3   <>"     "<>    l 4   <>"     "<>    l 5   <>"     "<>    l 6   <>"     "<>    l 7   <>"     "<>    l 8   <>"   ",
     " "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "      "      <>     "    "]
     where p s = showPosition GameMode board (colour, s)
           l i = showLabel GameMode i
           i i = showIndex colour i


template :: DisplayMode -> Board -> Colour -> String
template mode board colour = unlines $ 
    [a   <> s <> p        <> p <> p        <> p <> p        <> p <> p        <> p <> p        <> p <> p        <> p <> p        <> p <> p        <> p,
     a   <> s <> l 1      <> s <> l 2      <> s <> l 3      <> s <> l 4      <> s <> l 5      <> s <> l 6      <> s <> l 7      <> s <> l 8      <> s,
     a   <> s <> p        <> s <> p        <> s <> p        <> s <> p        <> s <> p        <> s <> p        <> s <> p        <> s <> p        <> s,
     
     a   <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m,
     i 8 <> m <> e (1, 8) <> m <> e (2, 8) <> m <> e (3, 8) <> m <> e (4, 8) <> m <> e (5, 8) <> m <> e (6, 8) <> m <> e (7, 8) <> m <> e (8, 8) <> m,
     a   <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m,

     a   <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m,
     i 7 <> m <> e (1, 7) <> m <> e (2, 7) <> m <> e (3, 7) <> m <> e (4, 7) <> m <> e (5, 7) <> m <> e (6, 7) <> m <> e (7, 7) <> m <> e (8, 7) <> m,
     a   <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m,

     a   <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m,
     i 6 <> m <> e (1, 6) <> m <> e (2, 6) <> m <> e (3, 6) <> m <> e (4, 6) <> m <> e (5, 6) <> m <> e (6, 6) <> m <> e (7, 6) <> m <> e (8, 6) <> m,
     a   <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m,

     a   <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m <> t        <> m,
     i 5 <> m <> e (1, 5) <> m <> e (2, 5) <> m <> e (3, 5) <> m <> e (4, 5) <> m <> e (5, 5) <> m <> e (6, 5) <> m <> e (7, 5) <> m <> e (8, 5) <> m,
     a   <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m <> b        <> m,

     a   <> s <> t        <> s <> t        <> s <> t        <> s <> t        <> s <> t        <> s <> t        <> s <> t        <> s <> t        <> s,
     a   <> s <> l 1      <> s <> l 2      <> s <> l 3      <> s <> l 4      <> s <> l 5      <> s <> l 6      <> s <> l 7      <> s <> l 8      <> s]
    where le = maybe 0 (length . showPiece mode . figure)
             $ maxBy (length . showPiece mode . figure) 
             $ coordinates board                                         -- largest string entry
          ll = length $ showLabel mode 1                                 -- largest string label
          li = length $ showIndex colour 1                               -- largest string index
          m = "|"                                                        -- delimiter entry
          e s = showPosition mode board (colour, s)                      -- lookup entry
          l  = centerOn le $ showLabel mode                              -- lookup label => center and pad it based on the largest string entry
          i  = showIndex colour                                          -- lookup index
          t  = stringOf "‾" le                                           -- top line => dependent on largest string entry
          b  = stringOf " " le                                           -- bottom pad => dependent on largest string entry
          a  = stringOf " " li                                           -- index pad => dependent on largest string index
          p = stringOf " "  ll                                           -- label pad => dependent on the largest string label
          s = stringOf " " $ length m                                    -- stub pad => dependent on string delimiter size
          centerOn max l = l