-- Long Algebraic Notation Parser
module Parser.LAN (moveParser, appliedMoveParser, stockfishMoveParser, parse, parseApply) where

import qualified Text.Megaparsec as M
import qualified Chess.Internal as Chess
import Text.Megaparsec (Parsec, (<|>), runParser, try, many, choice)
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
              | ShortCastle deriving (Ord, Eq)

data LANError = KnownError MoveType Chess.Position Chess.Coord 
              | UnknownError MoveType Chess.Square Chess.Coord  
              | IllegalError Chess.Move deriving (Ord, Eq)

instance (M.ShowErrorComponent LANError) where
    showErrorComponent = showLanError

instance Show LANError where
    show = showLanError

type Parser a = Parsec LANError String a

type LANParseError = ParseError LANError

showLanError :: LANError -> String
showLanError (KnownError (Advance) position coord)     = "Cannot advance with " <> show position <> " to " <> show coord
showLanError (KnownError (Capture) position coord)     = "Cannot capture with " <> show position <> " at " <> show coord
showLanError (KnownError (Enpassant) position coord)   = "Cannot capture enpassant from " <> show position <> " at " <> show coord
showLanError (KnownError (Progress) position coord)    = "Cannot progress from " <> show position <> " to " <> show coord
showLanError (KnownError (Promote p) position coord)   = "Cannot promote to " <> show p <> " at " <> show coord
showLanError (KnownError (LongCastle) position coord)  = "Cannot castle long"  
showLanError (KnownError (ShortCastle) position coord) = "Cannot castle short"
    
showLanError (UnknownError (Advance) (_, s) coord)     = "Cannot advance from " <> show s <> " to " <> show coord
showLanError (UnknownError (Capture) (_, s) coord)     = "Cannot capture from " <> show s <> " to " <> show coord
showLanError (UnknownError (Enpassant) (_, s) coord)   = "Cannot capture enpassant from " <> show s <> " to " <> show coord
showLanError (UnknownError (Progress) (_, s) coord)    = "Cannot progress from " <> show s <> " to " <> show coord
showLanError (UnknownError (Promote p) (_, s) coord)   = "Cannot promote to " <> show p <> " at " <> show coord
showLanError (UnknownError (LongCastle) (_, s) coord)  = "Cannot castle long"
showLanError (UnknownError (ShortCastle) (_, s) coord) = "Cannot castle short"

knownError :: MoveType -> Chess.Figure -> Chess.Coord -> Chess.Coord -> LANError
knownError m (p, c) s = KnownError m (Chess.Pos p c s)

unknownError :: MoveType -> Chess.Colour -> Chess.Coord -> Chess.Coord -> LANError
unknownError m c s = UnknownError m (c, s)

promote :: Chess.Figure -> [Chess.Move] -> Parser Chess.Move
promote (p, c) moves = do
    sx <- file
    sy <- rank
    ex <- file
    ey <- rank
    np <- promotions (c, (ex, ey))
    let newPiece = Chess.piece np
    failWith (knownError (Promote newPiece) (p, c) (sx, sy) (ex, ey)) $ find (promotesAs np (sx, sy)) moves

promoteCapture :: Chess.Figure -> [Chess.Move] -> Parser Chess.Move
promoteCapture (p, c) moves = do
    sx <- file
    sy <- rank
    _  <- char' 'x'
    ex <- file
    ey <- rank
    np <- promotions (c, (ex, ey))
    let newPiece = Chess.piece np
    failWith (knownError (Promote newPiece) (p, c) (sx, sy) (ex, ey)) $ find (promotesAs np (sx, sy)) moves

enpassant :: Chess.Figure -> [Chess.Move] -> Parser Chess.Move
enpassant figure moves = do
    sx <- file
    sy <- rank
    _  <- char' 'x'
    ex <- file
    ey <- rank
    failWith (knownError Enpassant figure (sx, sy) (ex, ey)) $ find (every [enpassantAt (ex, ey), hasCoord (sx, sy)]) moves

advance :: Chess.Figure ->[Chess.Move] -> Parser Chess.Move
advance figure moves = do
    sx <- file
    sy <- rank
    ex <- file
    ey <- rank
    failWith (knownError Advance figure (sx, sy) (ex, ey)) $ find (every [advancesTo (ex, ey), hasCoord (sx, sy)]) moves

capture :: Chess.Figure -> [Chess.Move] -> Parser Chess.Move
capture figure moves = do
    sx <- file
    sy <- rank
    _  <- char' 'x'
    ex <- file
    ey <- rank
    failWith (knownError Capture figure (sx, sy) (ex, ey)) $ find (every [capturesAt (ex, ey), hasCoord (sx, sy)]) moves

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

shortCastle :: Chess.Figure -> [Chess.Move] -> Parser Chess.Move
shortCastle figure moves = choice [try $ (string' "e1g1" >> failFor Chess.W), 
                                   try $ (string' "e8g8" >> failFor Chess.B)]
    where failFor Chess.W  = failed (5,1) (7,1)
          failFor Chess.B  = failed (5,8) (7,8)
          failed s e       = failWith (knownError ShortCastle figure s e) $ find (castlesTowards Chess.R) moves

longCastle :: Chess.Figure -> [Chess.Move] -> Parser Chess.Move
longCastle figure moves = choice [try $ (string' "e1c1" >> failFor Chess.W), 
                                  try $ (string' "e8c8" >> failFor Chess.B)]
    where failFor Chess.W = failed (5,1) (3,1)
          failFor Chess.B = failed (5,8) (3,8)
          failed s e      = failWith (knownError LongCastle figure s e) $ find (castlesTowards Chess.L) moves

king :: Chess.Board -> Parser Chess.Move
king board = choice [try $ char' 'K' >> (captureOrAdvance figure moves),
                     try $ shortCastle figure moves,
                     try $ longCastle figure moves]
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
    failWith (unknownError Progress colour start end) $ find (oneOf [every [capturesAt end, hasCoord start],
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
                              try $ shortCastle king moves,
                              longCastle king moves]
        where colour = Chess.player board
              pawn   = (Chess.Pawn, colour)
              king   = (Chess.King, colour) 
              moves  = Chess.movesColour board colour

applied :: Chess.Board -> Chess.Move -> Parser Chess.Board
applied board move = maybe illegal return $ Chess.permitApply board move
    where illegal = failWith (IllegalError move) Nothing

-- do i need to look out for checks and mates? (probably)
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
    _ <- delimitation
    return b

stockfishMoveParser :: Chess.Board -> Parser Chess.Move
stockfishMoveParser board = do
    _ <- delimitation
    m <- stockfishMove board
    _ <- delimitation
    return m

parse :: Chess.Board -> String -> Either LANParseError Chess.Move
parse board = run (moveParser board)

parseApply :: Chess.Board -> String -> Either LANParseError Chess.Board
parseApply board = run (appliedMoveParser board)