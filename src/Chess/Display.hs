module Chess.Display where

import Chess.Internal (Piece (King, Queen, Rook, Bishop, Knight, Pawn, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Colour(W, B), Position (Pos),
                       Castles (Both, Long, Short, None), 
                       Square, Figure, Coord, Board, player, lookAt, figure, coordinates, other, colour)
import Data.Maybe (maybe)
import Lib.Coll (maxBy)

data DisplayMode = GameMode | DebugMode deriving (Eq, Show)

defaultMode = DebugMode

manyOf :: String -> Int -> String
manyOf a i = foldr (<>) "" $ take i $ repeat a

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

showFigure :: DisplayMode -> Figure -> String
showFigure GameMode  = gameFigure
showFigure DebugMode = debugFigure

gameFile :: Int -> String
gameFile 1 = "a"
gameFile 2 = "b"
gameFile 3 = "c"
gameFile 4 = "d"
gameFile 5 = "e"
gameFile 6 = "f"
gameFile 7 = "g"
gameFile 8 = "h"

debugFile :: Int -> String
debugFile = show

showFile :: DisplayMode -> Int -> String
showFile GameMode  = gameFile
showFile DebugMode = debugFile 

standardFigure :: Figure -> String
standardFigure (Pawn, _)   = "P"
standardFigure (Bishop, _) = "B"
standardFigure (Rook, _)   = "R"
standardFigure (Knight, _) = "N"
standardFigure (King, _)   = "K"
standardFigure (Queen, _)  = "Q"
standardFigure (Empty, _)  = ""

debugFigure :: Figure -> String
debugFigure (Pawn, W)   = "P (W)"
debugFigure (Pawn, B)   = "P (B)"
debugFigure (Bishop, W) = "B (W)"
debugFigure (Bishop, B) = "B (B)"
debugFigure (King, W)   = "K (W)"
debugFigure (King, B)   = "K (B)"
debugFigure (Rook, W)   = "R (W)"
debugFigure (Rook, B)   = "R (B)"
debugFigure (Knight, W) = "N (W)"
debugFigure (Knight, B) = "N (B)"
debugFigure (Queen, W)  = "Q (W)"
debugFigure (Queen, B)  = "Q (B)"
debugFigure (Empty, _)  = "-" 

showRank :: Colour -> Int -> String
showRank c i = show $ snd $ boardCoord $ (c, (1, i))

gameColour :: Colour -> String
gameColour W = "White"
gameColour B = "Black"

debugColour :: Colour -> String
debugColour W = "W"
debugColour B = "B"

showColour :: DisplayMode -> Colour -> String
showColour GameMode = gameColour
showColour DebugMode = debugColour

gamePosition :: Position -> String
gamePosition (Pos p c s) = gameFigure (p, c) <> show s

debugPosition :: Position -> String
debugPosition (Pos p c s) = debugFigure (p, c) <> show s

showPosition :: DisplayMode -> Position -> String
showPosition GameMode  = gamePosition
showPosition DebugMode = debugPosition

showSquare :: DisplayMode -> Board -> Square -> String
showSquare mode board = maybe (show Empty) (showFigure mode . figure) . lookAt board . boardCoord

gameCastles :: Castles -> String
gameCastles Both  = "O-O-O / O-O"
gameCastles Long  = "O-O-O"
gameCastles Short = "O-O"
gameCastles None  = "-"

debugCastles :: Castles -> String
debugCastles Both  = "Both"
debugCastles Long  = "Long"
debugCastles Short = "Short"
debugCastles None  = "-"

showCastles :: DisplayMode -> Castles -> String
showCastles GameMode  = gameCastles
showCastles DebugMode = debugCastles

gameMove :: Move -> String
gameMove (Capture pos s)                      = gamePosition pos <> " x " <> show s
gameMove (Advance pos s)                      = gamePosition pos <> " -> " <> show s
gameMove (Enpassant pos e enm)                = "(" <> gamePosition pos <> " -> "<> show e <> ")" <> " x " <> gamePosition enm
gameMove (Promote pos np (Pos Empty _ e))     = "(" <> gamePosition pos <> " -> " <> show e <> ")" <> " = " <> gameFigure (np, colour pos)
gameMove (Promote pos np (Pos p c s))         = "(" <> gamePosition pos <> " x " <> (gamePosition (Pos p c s)) <> ")" <> " = " <> gameFigure (np, colour pos)
gameMove (Castle (Pos _ _ (kx,_), (ex, _)) _) = if (ex < kx) then "O-O" else "O-O-O"

debugMove :: Move -> String
debugMove (Capture pos s)                      = "Capture " <> debugPosition pos <> " x " <> show s
debugMove (Advance pos s)                      = "Advance " <> debugPosition pos <> " -> " <> show s
debugMove (Enpassant pos e enm)                = "Enpassant " <> debugPosition pos <> " -> " <> show e <> " x " <> debugPosition enm
debugMove (Promote pos np (Pos Empty _ e))     = "Promote " <> debugPosition pos <> " " <> show e <> " = " <> debugFigure (np, colour pos)
debugMove (Promote pos np (Pos p c s))         = "Promote " <> debugPosition pos <> " x " <> (debugPosition (Pos p c s)) <> " = " <> debugFigure (np, colour pos)
debugMove (Castle (Pos _ _ (kx,_), (ex, _)) _) = if (ex < kx) then "Castles Long" else "Castles Short"

showMove :: DisplayMode -> Move -> String
showMove GameMode  = gameMove
showMove DebugMode = debugMove

padBy :: Int -> String -> String
padBy i s = halves <> s <> halves
    where half   = round $ (fromIntegral i / 2)
          halves = manyOf " " half

centerOn :: Int -> String -> String
centerOn width s = if (even delta) 
                    then halves <> s <> halves
                    else halves <> s <> thirds
    where delta  = width - (length s)
          half   = round $ (fromIntegral delta / 2)
          third  = delta - half
          halves = manyOf " " half
          thirds = manyOf " " third

gameBoard :: Board -> String
gameBoard board = template GameMode board (player board)

debugBoard :: Board -> String
debugBoard board = template DebugMode board W

showBoard :: DisplayMode -> Board -> String
showBoard GameMode  = gameBoard
showBoard DebugMode = debugBoard

template :: DisplayMode -> Board -> Colour -> String
template mode board colour = unlines $ 
    [ip  <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp,
     ip  <> dp <> l 1      <> dp <> l 2      <> dp <> l 3      <> dp <> l 4      <> dp <> l 5      <> dp <> l 6      <> dp <> l 7      <> dp <> l 8      <> dp,
     ip  <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp <> lp       <> dp,
     
     ip  <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d,
     i 8 <> d  <> e (1, 8) <> d  <> e (2, 8) <> d  <> e (3, 8) <> d  <> e (4, 8) <> d  <> e (5, 8) <> d  <> e (6, 8) <> d  <> e (7, 8) <> d  <> e (8, 8) <> d,
     ip  <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d,

     ip  <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d,
     i 7 <> d  <> e (1, 7) <> d  <> e (2, 7) <> d  <> e (3, 7) <> d  <> e (4, 7) <> d  <> e (5, 7) <> d  <> e (6, 7) <> d  <> e (7, 7) <> d  <> e (8, 7) <> d,
     ip  <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d,

     ip  <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d,
     i 6 <> d  <> e (1, 6) <> d  <> e (2, 6) <> d  <> e (3, 6) <> d  <> e (4, 6) <> d  <> e (5, 6) <> d  <> e (6, 6) <> d  <> e (7, 6) <> d  <> e (8, 6) <> d,
     ip  <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d,

     ip  <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d,
     i 5 <> d  <> e (1, 5) <> d  <> e (2, 5) <> d  <> e (3, 5) <> d  <> e (4, 5) <> d  <> e (5, 5) <> d  <> e (6, 5) <> d  <> e (7, 5) <> d  <> e (8, 5) <> d,
     ip  <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d,

     ip  <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d,
     i 4 <> d  <> e (1, 4) <> d  <> e (2, 4) <> d  <> e (3, 4) <> d  <> e (4, 4) <> d  <> e (5, 4) <> d  <> e (6, 4) <> d  <> e (7, 4) <> d  <> e (8, 4) <> d,
     ip  <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d,
     
     ip  <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d,
     i 3 <> d  <> e (1, 3) <> d  <> e (2, 3) <> d  <> e (3, 3) <> d  <> e (4, 3) <> d  <> e (5, 3) <> d  <> e (6, 3) <> d  <> e (7, 3) <> d  <> e (8, 3) <> d,
     ip  <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d,

     ip  <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d,
     i 2 <> d  <> e (1, 2) <> d  <> e (2, 2) <> d  <> e (3, 2) <> d  <> e (4, 2) <> d  <> e (5, 2) <> d  <> e (6, 2) <> d  <> e (7, 2) <> d  <> e (8, 2) <> d,
     ip  <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d,
     
     ip  <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d  <> t        <> d,
     i 1 <> d  <> e (1, 1) <> d  <> e (2, 1) <> d  <> e (3, 1) <> d  <> e (4, 1) <> d  <> e (5, 1) <> d  <> e (6, 1) <> d  <> e (7, 1) <> d  <> e (8, 1) <> d,
     ip  <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d  <> b        <> d,

     ip  <> dp <> t        <> dp <> t        <> dp <> t        <> dp <> t        <> dp <> t        <> dp <> t        <> dp <> t        <> dp <> t        <> dp,
     ip  <> dp <> l 1      <> dp <> l 2      <> dp <> l 3      <> dp <> l 4      <> dp <> l 5      <> dp <> l 6      <> dp <> l 7      <> dp <> l 8      <> dp]
    where le    = maybe 0 (length . pad . showFigure mode . figure)
                $ maxBy   (length . pad . showFigure mode . figure) 
                $ coordinates board                                            -- largest padded string entry
          ll    = length $ l 1                                                 -- largest string label
          li    = length $ i 1                                                 -- largest string index
          d     = "|"                                                          -- delimiter entry
          dp    = manyOf " " $ length d                                        -- delimiter pad => dependent on string delimiter size
          e     = centerOn le . pad . pos                                      -- lookup entry
          l     = centerOn le . showFile mode                                 -- lookup label => center and pad it based on the largest string entry
          i     = pad . showRank colour                                       -- lookup index
          t     = manyOf "‾" le                                                -- top       => dependent on largest string entry
          b     = manyOf " " le                                                -- bottom    => dependent on largest string entry
          ip    = manyOf " " li                                                -- index pad => dependent on largest string index
          lp    = manyOf " "  ll                                               -- label pad => dependent on the largest string label
          pad   = padBy 4
          pos s = showSquare mode board (colour, s)

-- FIXME: Redo these
-- showPieces :: Board -> IO ()
-- showPieces = putStrLn . unlines . join . map fanOut . M.toList . pieces
--       where fanOut (c, pmap) = [show c <> " :: "] <> (map row $ M.toList pmap)
--             row (p, cs) = "     " <> (show p) <> " - " <> (show cs)

-- statistics :: Board -> String
-- statistics board = unlines ["Player:     " <> show (player board),
--                             "In-Check:   " <> show (check board),
--                             "Can castle: " <> show (pickCastle $ player board)]
--       where pickCastle B = blackCastle board
--             pickCastle W = whiteCastle board

instance Show Board where
      show = showBoard defaultMode

instance Show Position where
      show = showPosition defaultMode

instance Show Colour where
      show = showColour defaultMode

instance Show Castles where
      show = showCastles defaultMode

instance Show Move where
      show = showMove defaultMode