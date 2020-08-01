-- Long Algebraic Notation Parser
module Parser.LAN where

import qualified Text.Megaparsec as M
import qualified Chess.Internal as Chess
import Text.Megaparsec (Parsec, (<|>), runParser, try, many, choice)
import Text.Megaparsec.Char (char, char', string, string', spaceChar, numberChar, asciiChar, newline)
import Data.List (find)
import Parser.Common
import Lib.Coll
import Chess.Display

data MoveType = Advance 
              | Capture 
              | Enpassant
              | Progress 
              | Promote Chess.Piece 
              | LongCastle 
              | ShortCastle deriving (Ord, Eq)

data LANError = KnownError MoveType Chess.Position Chess.Coord 
              | UnknownError MoveType Chess.Square Chess.Coord  deriving (Ord, Eq)

instance (M.ShowErrorComponent LANError) where
    showErrorComponent (KnownError (Advance) position coord)     = "Cannot advance with " <> show position <> " to " <> show coord
    showErrorComponent (KnownError (Capture) position coord)     = "Cannot capture with " <> show position <> " at " <> show coord
    showErrorComponent (KnownError (Enpassant) position coord)   = "Cannot capture enpassant from " <> show position <> " at " <> show coord
    showErrorComponent (KnownError (Progress) position coord)    = "Cannot progress from " <> show position <> " to " <>
    
     show coord
    showErrorComponent (KnownError (Promote p) position coord)   = "Cannot promote to " <> show p <> " at " <> show coord
    showErrorComponent (KnownError (LongCastle) position coord)  = "Cannot castle long"  
    showErrorComponent (KnownError (ShortCastle) position coord) = "Cannot castle short"
    
    showErrorComponent (UnknownError (Advance) (_, s) coord)     = "Cannot advance from " <> show s <> " to " <> show coord
    showErrorComponent (UnknownError (Capture) (_, s) coord)     = "Cannot capture from " <> show s <> " to " <> show coord
    showErrorComponent (UnknownError (Enpassant) (_, s) coord)   = "Cannot capture enpassant from " <> show s <> " to " <> show coord
    showErrorComponent (UnknownError (Progress) (_, s) coord)    = "Cannot progress from " <> show s <> " to " <> show coord
    showErrorComponent (UnknownError (Promote p) (_, s) coord)   = "Cannot promote to " <> show p <> " at " <> show coord
    showErrorComponent (UnknownError (LongCastle) (_, s) coord)  = "Cannot castle long"
    showErrorComponent (UnknownError (ShortCastle) (_, s) coord) = "Cannot castle short"
    

type Parser a = Parsec LANError String a

type LANParseError = ParseError LANError

lanError :: MoveType -> Chess.Board -> Chess.Coord -> Chess.Coord -> LANError
lanError move board s e = case (Chess.lookAt board s) of
    (Just p)  -> KnownError move p e
    (Nothing) -> UnknownError move (Chess.player board, s) e

advance :: Chess.Board -> [Chess.Move] -> Parser Chess.Move
advance board moves = do
    sx <- file
    sy <- rank
    ex <- file
    ey <- rank
    failWith (lanError Advance board (sx, sy) (ex, ey)) $ find (every [advancesTo (ex, ey), hasCoord (sx, sy)]) moves

capture :: Chess.Board -> [Chess.Move] -> Parser Chess.Move
capture board moves = do
    sx <- file
    sy <- rank
    _  <- char' 'x'
    ex <- file
    ey <- rank
    failWith (lanError Capture board (sx, sy) (ex, ey)) $ find (every [capturesAt (ex, ey), hasCoord (sx, sy)]) moves

enpassant :: Chess.Board -> [Chess.Move] -> Parser Chess.Move
enpassant board moves = do
    sx <- file
    sy <- rank
    _  <- char' 'x'
    ex <- file
    ey <- rank
    failWith (lanError Enpassant board (sx, sy) (ex, ey)) $ find (every [enpassantAt (ex, ey), hasCoord (sx, sy)]) moves

progress :: Chess.Board -> [Chess.Move] -> Parser Chess.Move
progress board moves = do
    sx <- file
    sy <- rank
    ex <- file
    ey <- rank
    failWith (lanError Progress board (sx, sy) (ex, ey)) $ find (oneOf [every [capturesAt (ex, ey), hasCoord (sx, sy)],
                                                                        every [advancesTo (ex, ey), hasCoord (sx, sy)],
                                                                        every [enpassantAt (ex, ey), hasCoord (sx, sy)]]) moves

promote :: Chess.Board -> [Chess.Move] -> Parser Chess.Move
promote board moves = do
    sx <- file
    sy <- rank
    ex <- file
    ey <- rank
    p  <- promotions (Chess.player board, (sx, sy))
    failWith (lanError (Promote $ Chess.piece p) board (sx, sy) (ex, ey)) $ find (promotesAs p (sx, sy)) moves


shortCastle :: Chess.Board -> [Chess.Move] -> Parser Chess.Move
shortCastle board moves = choice [try $ (string' "e1g1" >> failFor Chess.W), 
                                  try $ (string' "e8g8" >> failFor Chess.B)]
    where failFor Chess.W  = failed (5,1) (7,1)
          failFor Chess.B  = failed (5,8) (7,8)
          failed s e       = failWith (lanError ShortCastle board s e) $ find (castlesTowards Chess.R) moves

longCastle :: Chess.Board -> [Chess.Move] -> Parser Chess.Move
longCastle board moves = choice [try $ (string' "e1c1" >> failFor Chess.W), 
                                 try $ (string' "e8c8" >> failFor Chess.B)]
    where failFor Chess.W = failed (5,1) (3,1)
          failFor Chess.B = failed (5,8) (3,8)
          failed s e      = failWith (lanError LongCastle board s e) $ find (castlesTowards Chess.L) moves

castle :: Chess.Board -> [Chess.Move] -> Parser Chess.Move
castle board moves = choice [try $ shortCastle board moves, try $ longCastle board moves]


-- moves also may contain pieces as far as i could tell
move :: Chess.Board -> Parser Chess.Move
move board = choice [try $ advance board moves, 
                     try $ capture board moves, 
                     try $ enpassant board moves, 
                     try $ promote board moves, 
                     castle board moves]
        where moves = Chess.movesColour board (Chess.player board)

stockfishMove :: Chess.Board -> Parser Chess.Move
stockfishMove board = choice [try $ progress board moves,
                              try $ promote board moves,
                              castle board moves]
        where moves = Chess.movesColour board (Chess.player board)


moveParser :: Chess.Board -> Parser Chess.Move
moveParser board = do
    _ <- delimitation
    m <- move board
    _ <- delimitation
    return m

stockfishMoveParser :: Chess.Board -> Parser Chess.Move
stockfishMoveParser board = do
    _ <- delimitation
    m <- stockfishMove board
    _ <- delimitation
    return m