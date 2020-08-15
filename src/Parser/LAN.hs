-- Long Algebraic Notation Parser
module Parser.LAN (moveParser, appliedMoveParser, stockfishMoveParser, parse, parseApply) where

import qualified Text.Megaparsec as M
import qualified Chess.Internal as Chess
import Text.Megaparsec (Parsec, (<|>), runParser, try, many, choice, single, optional)
import Text.Megaparsec.Char (char, char', string, string', spaceChar, numberChar, asciiChar, newline)
import Data.List (find)
import Parser.Common
import Lib.Coll
import Lib.Megaparsec
import Chess.Display

data MoveType = Advance 
              | Capture 
              | Enpassant
              | Progress 
              | Promote Chess.Piece
              | LongCastle 
              | ShortCastle 
              deriving (Ord, Eq)

data Details = Total Chess.Position Chess.Coord
             | Partial Chess.Square Chess.Coord
             deriving (Ord, Eq)

data LANError = ImpossibleMoveError MoveType Details
              | IllegalMoveError Chess.Move
              | UnexpectedCheckError 
              deriving (Ord, Eq)

instance (M.ShowErrorComponent LANError) where
    showErrorComponent = show

instance Show LANError where
    show (ImpossibleMoveError (Advance) details)     = "Cannot advance: " <> show details
    show (ImpossibleMoveError (Capture) details)     = "Cannot capture: " <> show details
    show (ImpossibleMoveError (Enpassant) details)   = "Cannot capture enpassant: " <> show details
    show (ImpossibleMoveError (Progress) details)    = "Cannot progress " <> show details
    show (ImpossibleMoveError (Promote p) details)   = "Cannot promote to " <> show p <> ": " <> show details
    show (ImpossibleMoveError (LongCastle) details)  = "Cannot castle long"
    show (ImpossibleMoveError (ShortCastle) details) = "Cannot castle short"
    show (IllegalMoveError move)                     = "Illegal move: " <> show move
    show (UnexpectedCheckError)                      = "Check not allowed. Board is not in check."


instance Show Details where
    show (Total (Chess.Pos p c s) e) = showFigure ErrorMode (p, c) <> " cannot go from " <> showCoord ErrorMode (c, s) <> " to " <> showCoord ErrorMode (c, e)
    show (Partial (c, s) e)          = showColour ErrorMode c <> " cannot go from " <> showCoord ErrorMode (c, s) <> " to " <> showCoord ErrorMode (c, e)

type Parser a = Parsec LANError String a

type LANParseError = ParseError LANError

check :: Chess.Board -> Parser ()
check board = (optional $ single '+') >>= verify
    where verify (Just _) | Chess.check board = return ()
          verify (Nothing)                    = return ()
          verify (Just _)                     = failWith UnexpectedCheckError Nothing

totalError :: MoveType -> Chess.Position -> Chess.Coord -> LANError
totalError move position = ImpossibleMoveError move . Total position

partialError :: MoveType -> Chess.Square -> Chess.Coord -> LANError
partialError move square = ImpossibleMoveError move . Partial square

promote :: Chess.Figure -> [Chess.Move] -> Parser Chess.Move
promote (p, c) moves = do
    sx <- file
    sy <- rank
    ex <- file
    ey <- rank
    _  <- char '='
    np <- promotions (c, (ex, ey))
    let newPiece = Chess.piece np
    let error    = totalError (Promote newPiece) (Chess.Pos p c (sx, sy)) (ex, ey)
    failWith error $ find (promotesAs np (sx, sy)) moves

promoteCapture :: Chess.Figure -> [Chess.Move] -> Parser Chess.Move
promoteCapture (p, c) moves = do
    sx <- file
    sy <- rank
    _  <- char' 'x'
    ex <- file
    ey <- rank
    _  <- char '='
    np <- promotions (c, (ex, ey))
    let newPiece = Chess.piece np
    let error    = totalError (Promote newPiece) (Chess.Pos p c (sx, sy)) (ex, ey)
    failWith error $ find (promotesAs np (sx, sy)) moves

enpassant :: Chess.Figure -> [Chess.Move] -> Parser Chess.Move
enpassant (p, c) moves = do
    sx <- file
    sy <- rank
    _  <- char' 'x'
    ex <- file
    ey <- rank
    let error = totalError Enpassant (Chess.Pos p c (sx ,sy)) (ex, ey) 
    failWith error $ find (every [enpassantAt (ex, ey), hasCoord (sx, sy)]) moves

advance :: Chess.Figure ->[Chess.Move] -> Parser Chess.Move
advance (p, c) moves = do
    sx <- file
    sy <- rank
    _  <- M.optional (char '-')
    ex <- file
    ey <- rank
    let error = totalError Advance (Chess.Pos p c (sx, sy)) (ex, ey)
    failWith error $ find (every [advancesTo (ex, ey), hasCoord (sx, sy)]) moves

capture :: Chess.Figure -> [Chess.Move] -> Parser Chess.Move
capture (p, c) moves = do
    sx <- file
    sy <- rank
    _  <- char' 'x'
    ex <- file
    ey <- rank
    let error = totalError Capture (Chess.Pos p c (sx, sy)) (ex, ey)
    failWith error $ find (every [capturesAt (ex, ey), hasCoord (sx, sy)]) moves

captureOrAdvance :: Chess.Figure -> [Chess.Move] -> Parser Chess.Move
captureOrAdvance figure moves = choice [try $ advance figure moves, try $ capture figure moves]

pawn :: Chess.Board -> Parser Chess.Move
pawn board = choice [try $ promote figure moves,
                     try $ promoteCapture figure moves,
                     try $ capture figure moves,
                     try $ enpassant figure moves,
                     try $ advance figure moves]
    where figure = (Chess.Pawn, Chess.player board)
          moves = Chess.movesPiece board figure

rook :: Chess.Board -> Parser Chess.Move
rook board = char' 'R' >> (captureOrAdvance figure moves)
    where figure = (Chess.Rook, Chess.player board)
          moves = Chess.movesPiece board figure

knight :: Chess.Board -> Parser Chess.Move
knight board = char' 'N' >> (captureOrAdvance figure moves)
    where figure = (Chess.Knight, Chess.player board) 
          moves = Chess.movesPiece board figure

bishop :: Chess.Board -> Parser Chess.Move
bishop board = char' 'B' >> (captureOrAdvance figure moves)
    where figure = (Chess.Bishop, Chess.player board)
          moves  = Chess.movesPiece board figure

queen :: Chess.Board -> Parser Chess.Move
queen board = char' 'Q' >> (captureOrAdvance figure moves)
    where figure = (Chess.Queen, Chess.player board)
          moves  = Chess.movesPiece board figure

castle :: Chess.Castles -> Chess.Figure -> [Chess.Move] -> Parser Chess.Move
castle Chess.Long  (k, Chess.W) = failWith (totalError LongCastle (Chess.Pos k Chess.W (5, 1)) (7, 1)) . find (castlesTowards Chess.L)
castle Chess.Short (k, Chess.W) = failWith (totalError ShortCastle (Chess.Pos k Chess.W (5, 1)) (3, 1)) . find (castlesTowards Chess.R)
castle Chess.Long  (k, Chess.B) = failWith (totalError LongCastle (Chess.Pos k Chess.B (5, 8)) (7, 8)) . find (castlesTowards Chess.L)
castle Chess.Short (k, Chess.B) = failWith (totalError ShortCastle (Chess.Pos k Chess.B (5, 8)) (3, 8)) . find (castlesTowards Chess.R)

king :: Chess.Board -> Parser Chess.Move
king board = choice [try $ char' 'K' >> (captureOrAdvance figure moves),
                     try $ string' "O-O-O" >> (castle Chess.Long figure moves),
                     try $ string' "O-O" >> (castle Chess.Short figure moves)]
    where figure = (Chess.King, Chess.player board)
          moves  = Chess.movesPiece board figure

progress :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
progress colour moves = do
    sx <- file
    sy <- rank
    ex <- file
    ey <- rank
    let start = (sx, sy)
    let end   = (ex, ey)
    let error = partialError Progress (colour, start) end
    failWith error $ find (oneOf [every [capturesAt end, hasCoord start],
                                  every [advancesTo end, hasCoord start],
                                  every [enpassantAt end, hasCoord start]]) moves
                                                                        
move :: Chess.Board -> Parser Chess.Move
move board = choice [try $ pawn board, 
                     try $ knight board,
                     try $ bishop board, 
                     try $ queen board,
                     try $ rook board,
                     king board]
        where moves = Chess.movesColour board (Chess.player board)

stockfishMove :: Chess.Board -> Parser Chess.Move
stockfishMove board = choice [try $ progress colour moves,
                              try $ promote pawn moves,
                              king board]
        where colour = Chess.player board
              pawn   = (Chess.Pawn, colour) 
              moves  = Chess.movesColour board colour

applied :: Chess.Board -> Chess.Move -> Parser Chess.Board
applied board move = maybe illegal return $ Chess.permitApply board move
    where illegal = failWith (IllegalMoveError move) Nothing

moveParser :: Chess.Board -> Parser Chess.Move
moveParser board = do
    _ <- delimitation
    m <- move board
    _ <- delimitation
    return m

appliedMoveParser :: Chess.Board -> Parser Chess.Board
appliedMoveParser board = do
    m <- moveParser board
    b <- applied board m
    _ <- check b
    _ <- mate
    _ <- delimitation
    return b

stockfishMoveParser :: Chess.Board -> Parser Chess.Move
stockfishMoveParser board = do
    _ <- delimitation
    m <- stockfishMove board
    _ <- delimitation
    return m

-- https://en.wikipedia.org/wiki/Chess_notation -> website with examples (LAN includes checks)
parse :: Chess.Board -> String -> Either LANParseError Chess.Move
parse board = run (moveParser board)

parseApply :: Chess.Board -> String -> Either LANParseError Chess.Board
parseApply board = run (appliedMoveParser board)