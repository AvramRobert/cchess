module Chess.Display where

import Chess.Internal (Piece (King, Queen, Rook, Bishop, Knight, Pawn, Empty),
                       Move (Capture, Advance, Enpassant, Promote, Castle),
                       Colour(W, B), Position (Pos),
                       Castles (Both, Long, Short, None), 
                       Square, Figure, Coord, Board, player, lookAt, figure, coordinates, other, colour)
import qualified Chess.Game as G
import qualified PGN.Writer as W
import Data.Maybe (maybe)
import Lib.Coll (maxBy, chunksOf)
import Data.List (intersperse, find)

data DisplayMode = GameMode | DebugMode | ErrorMode deriving (Eq, Show)

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

debugFigure :: Figure -> String
debugFigure (Pawn, _)   = "P"
debugFigure (Bishop, _) = "B"
debugFigure (Rook, _)   = "R"
debugFigure (Knight, _) = "N"
debugFigure (King, _)   = "K"
debugFigure (Queen, _)  = "Q"
debugFigure (Empty, _)  = ""

errorFigure :: Figure -> String
errorFigure (Pawn, c)    = "Pawn ("   <> errorColour c <> ")"
errorFigure (Bishop, c)  = "Bishop (" <> errorColour c <> ")"
errorFigure (Rook, c)    = "Rook ("   <> errorColour c <> ")"
errorFigure (Knight, c)  = "Knight (" <> errorColour c <> ")"
errorFigure (King, c)    = "King ("   <> errorColour c <> ")"
errorFigure (Queen, c)   = "Queen ("  <> errorColour c <> ")"
errorFigure (Empty, c)   = "-"

showFigure :: DisplayMode -> Figure -> String
showFigure GameMode  = gameFigure
showFigure DebugMode = debugFigure
showFigure ErrorMode = errorFigure

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

errorFile = gameFile

showFile :: DisplayMode -> Int -> String
showFile GameMode  = gameFile
showFile DebugMode = debugFile
showFile ErrorMode = errorFile 

gameRank :: Square -> String
gameRank = show . snd . boardCoord

debugRank :: Square -> String
debugRank = show . snd . snd

errorRank :: Square -> String
errorRank = debugRank

showRank :: DisplayMode -> Square -> String
showRank GameMode  = gameRank
showRank DebugMode = debugRank
showRank ErrorMode = errorRank

gameCoord :: Square -> String
gameCoord square = gameFile (fst $ snd square) <> gameRank square

debugCoord :: Square -> String
debugCoord square = debugFile (fst $ snd square) <> debugRank square

errorCoord :: Square -> String
errorCoord = gameCoord

showCoord :: DisplayMode -> Square -> String
showCoord GameMode  = gameCoord
showCoord DebugMode = debugCoord
showCoord ErrorMode = errorCoord

gameColour :: Colour -> String
gameColour W = "White"
gameColour B = "Black"

debugColour :: Colour -> String
debugColour W = "W"
debugColour B = "B"

errorColour :: Colour -> String
errorColour W = "White"
errorColour B = "Black"

showColour :: DisplayMode -> Colour -> String
showColour GameMode  = gameColour
showColour DebugMode = debugColour
showColour ErrorMode = errorColour 

gamePosition :: Position -> String
gamePosition (Pos p c s)  = debugFigure (p, c) <> gameCoord (c, s)

debugPosition :: Position -> String
debugPosition (Pos p c s) = debugFigure (p, c) <> debugCoord (c, s)

errorPosition :: Position -> String
errorPosition (Pos p c s) = debugFigure (p, c) <> gameCoord (c, s)

showPosition :: DisplayMode -> Position -> String
showPosition GameMode  = gamePosition
showPosition DebugMode = debugPosition
showPosition ErrorMode = errorPosition

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

errorCastles :: Castles -> String
errorCastles = debugCastles

showCastles :: DisplayMode -> Castles -> String
showCastles GameMode  = gameCastles
showCastles DebugMode = debugCastles
showCastles ErrorMode = errorCastles

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

errorMove :: Move -> String
errorMove = debugMove

showMove :: DisplayMode -> Move -> String
showMove GameMode  = gameMove
showMove DebugMode = debugMove
showMove ErrorMode = errorMove

showPGNMove :: Move -> Board -> Maybe String
showPGNMove = W.writeMove

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

gameOutcome :: G.Outcome -> String
gameOutcome (G.Win W) = "1-0"
gameOutcome (G.Win B) = "0-1"
gameOutcome (G.Draw)  = "1/2-1/2"
gameOutcome (G.Other) = "*"
            
taggedAs :: (G.Tag -> Maybe (String, String)) -> G.Tag -> String
taggedAs f tag = bracket $ maybe (standard tag) id $ f tag
      where standard (G.Event s)        = ("Event", s)
            standard (G.Site  s)        = ("Site", s)
            standard (G.Date  s)        = ("Date", s)
            standard (G.Round s)        = ("Round", s)
            standard (G.White s)        = ("White", s)
            standard (G.Black s)        = ("Black", s)
            standard (G.Result s)       = ("Result", gameOutcome s)
            standard (G.WhiteElo r)     = ("WhiteElo", elo r)
            standard (G.BlackElo r)     = ("BlackElo", elo r)
            standard (G.WhiteTitle t)   = ("Title", show t)
            standard (G.BlackTitle t)   = ("Title", show t)
            standard (G.WhiteUSCF s)    = ("WhiteUSCF", s)
            standard (G.BlackUSCF s)    = ("BlackUSCF", s)
            standard (G.WhiteNA s)      = ("WhiteNA", address s)
            standard (G.BlackNA s)      = ("BlackNA", address s)
            standard (G.WhiteType s)    = ("WhiteType", show s)
            standard (G.BlackType s)    = ("BlackType", show s)
            standard (G.EventDate s)    = ("EventDate", s)
            standard (G.EventSponsor s) = ("EventSponsor", s)
            standard (G.Section s)      = ("Section", s)
            standard (G.Stage s)        = ("Stage", s)
            standard (G.Board s)        = ("Board", s)
            standard (G.Opening s)      = ("Opening", s)
            standard (G.Variation s)    = ("Variation", s)
            standard (G.SubVariation s) = ("SubVariation", s)
            standard (G.ECO s)          = ("ECO", s)
            standard (G.NIC s)          = ("NIC", s)
            standard (G.Time s)         = ("Time", s)
            standard (G.UTCTime s)      = ("UTCTime", s)
            standard (G.UTCDate s)      = ("UTCDate", s)
            standard (G.TimeControl s)  = ("TimeControl", s)
            standard (G.SetUp s)        = ("SetUp", s)
            standard (G.FEN s)          = ("FEN", s)
            standard (G.Termination s)  = ("Termination", show s)
            standard (G.PlyCount s)     = ("PlyCount", s)
            standard (G.Annotator s)    = ("Annotator", s)
            standard (G.Mode s)         = ("Mode", show s)
            standard (G.Unknown t c)    = (t, c)
            bracket (title, value)      = "[" <> title <> " \"" <> value <> "\"]"
            outcome (G.Other)           = "*"
            elo (G.Rated r)             = r
            elo (G.Unrated)             = "-"
            address (G.Address a)       = a
            address (G.NoAddress)       = "-"

gameTag :: G.Tag -> String
gameTag tag = taggedAs normal tag
      where normal (G.Termination G.Checkmate)   = Just ("Termination", "Normal")
            normal (G.Termination G.Stalemate)   = Just ("Termination", "Normal")
            normal (G.Termination G.Resignation) = Just ("Termination", "Normal")
            normal (G.Termination r)             = Just ("Termination", show r)
            normal  _                            = Nothing
 
debugTag :: G.Tag -> String
debugTag tag = taggedAs (const Nothing) tag

showTag :: DisplayMode -> G.Tag -> String
showTag GameMode  = gameTag
showTag DebugMode = debugTag
showTag ErrorMode = gameTag

showGameBoard :: DisplayMode -> G.Game -> String
showGameBoard mode = showBoard mode . G.board

showGame :: DisplayMode -> G.Game -> String
showGame _ game = unlines (tags <> padding <> moves <> outcome <> padding)
      where tags     = fmap (showTag GameMode) $ G.tags game
            moves    = map (foldr (<>) "" . intersperse " ") $ chunksOf 6 $ W.writeMoves $ G.board game
            padding  = ["", ""]
            outcome  = maybe (error "This should never happen") (return . gameOutcome . extract) $ find result (G.tags game)
            result (G.Result _)  = True
            result _             = False
            extract (G.Result o) = o

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
          l     = centerOn le . showFile mode                                  -- lookup label => center and pad it based on the largest string entry
          i y   = pad $ showRank mode (colour, (1, y))                         -- lookup index
          t     = manyOf "‾" le                                                -- top       => dependent on largest string entry
          b     = manyOf " " le                                                -- bottom    => dependent on largest string entry
          ip    = manyOf " " li                                                -- index pad => dependent on largest string index
          lp    = manyOf " "  ll                                               -- label pad => dependent on the largest string label
          pad   = padBy 4
          pos s = showSquare mode board (colour, s)

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

instance Show G.Game where
      show = showGame defaultMode