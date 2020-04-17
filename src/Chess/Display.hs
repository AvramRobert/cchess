module Chess.Display where

import Chess.Internal (Piece (King, Queen, Rook, Bishop, Knight, Pawn, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Outcome (Illegal, Draw, Stalemate, Checkmate),  
                       Colour(W, B), Position (Pos),
                       Castles (Both, Long, Short, None), 
                       Square, Figure, Coord, Board, player, lookAt, figure, coordinates, other)
import Data.Maybe (maybe)
import Lib.Coll (maxBy)

data DisplayMode = GameMode | DebugMode deriving (Eq, Show)

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

showLabel :: DisplayMode -> Int -> String
showLabel GameMode  = gameLabel
showLabel DebugMode = debugLabel 

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

showIndex :: Colour -> Int -> String
showIndex c i = show $ snd $ boardCoord $ (c, (1, i))

showPiece :: DisplayMode -> Figure -> String
showPiece GameMode  = gameFigure
showPiece DebugMode = debugFigure

gameColour :: Colour -> String
gameColour W = "White"
gameColour B = "Black"

debugColour :: Colour -> String
debugColour W = "W"
debugColour B = "B"

showColour :: DisplayMode -> Colour -> String
showColour GameMode = gameColour
showColour DebugMode = debugColour

showPosition :: DisplayMode -> Position -> String
showPosition mode (Pos p c s) = showPiece mode (p, c) <> show s

showSquare :: DisplayMode -> Board -> Square -> String
showSquare mode board square = maybe (show Empty) (showPiece mode . figure) $ lookAt board $ boardCoord square

gameOutcome :: Outcome -> String
gameOutcome (Illegal move)    = "Illegal move"
gameOutcome (Draw board)      = "Draw"
gameOutcome (Stalemate board) = "Stalemate"
gameOutcome (Checkmate board) = "Checkmate" 

debugOutcome :: Outcome -> String
debugOutcome (Illegal move)    = "Illegal"
debugOutcome (Draw board)      = "Draw"
debugOutcome (Stalemate board) = "Stalemate"
debugOutcome (Checkmate board) = "Checkmate"

showOutcome :: DisplayMode -> Outcome -> String
showOutcome GameMode  = gameOutcome
showOutcome DebugMode = debugOutcome

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
    where le    = maybe 0 (length . pad . showPiece mode . figure)
                $ maxBy   (length . pad . showPiece mode . figure) 
                $ coordinates board                                            -- largest padded string entry
          ll    = length $ l 1                                                 -- largest string label
          li    = length $ i 1                                                 -- largest string index
          d     = "|"                                                          -- delimiter entry
          dp    = manyOf " " $ length d                                        -- delimiter pad => dependent on string delimiter size
          e     = centerOn le . pad . pos                                      -- lookup entry
          l     = centerOn le . showLabel mode                                 -- lookup label => center and pad it based on the largest string entry
          i     = pad . showIndex colour                                       -- lookup index
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

showBoard :: DisplayMode -> Board -> String
showBoard GameMode board = template GameMode board (player board)
showBoard DebugMode board = template DebugMode board W

-- FIXME: There should be a show for Castles
instance Show Board where
      show = showBoard DebugMode

instance Show Position where
      show = showPosition DebugMode

instance Show Move where
      show (Capture pos s)                      = show pos <> " x " <> show s
      show (Advance pos s)                      = show pos <> " -> " <> show s
      show (Enpassant pos e enm)                = "(" <> show pos <> " -> "<> show e <> ")" <> " x " <> show enm
      show (Promote pos np (Pos Empty _ e))     = "(" <> show pos <> " -> " <> show e <> ")" <> " = " <> show np
      show (Promote pos np (Pos p c s))         = "(" <> show pos <> " x " <> (show (Pos p c s)) <> ")" <> " = " <> show np
      show (Castle (Pos _ _ (kx,_), (ex, _)) _) = if (ex < kx) then "O-O" else "O-O-O"

instance Show Colour where
      show = showColour DebugMode

instance Show Outcome where
      show = showOutcome DebugMode

instance Show Castles where
      show = showCastles DebugMode