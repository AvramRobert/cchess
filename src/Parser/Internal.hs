module Parser.Internal where

import qualified Chess.Internal as Chess
import qualified Data.Set as S
import qualified Text.Megaparsec as M
import Data.Monoid (Monoid)
import Data.ByteString.Lazy (ByteString)
import Text.Megaparsec (Parsec, (<|>), runParser, try, many)
import Text.Megaparsec.Char (char, string, spaceChar, numberChar, asciiChar, newline)
import Data.Char (digitToInt)
import Control.Monad (void)
import Data.Functor (($>))
import Data.List (find)
import Lib.Coll
import Chess.Display

data ChessError = CaptureError Chess.Coord Chess.Piece |
                  AdvanceError Chess.Coord Chess.Piece |
                  PromoteError Chess.Coord Chess.Piece |
                  CastleError  Chess.Castles  | 
                  GameError    Chess.Outcome  |
                  MissingMovesError
                  deriving (Eq, Show, Ord)

type ParseError = M.ParseErrorBundle String ChessError

instance M.ShowErrorComponent ChessError where
    showErrorComponent (CaptureError coord piece) = "Could not capture at " <> (show coord) <> " with " <> (show piece)
    showErrorComponent (AdvanceError coord piece) = "Could not advance to " <> (show coord) <> " with " <> (show piece)
    showErrorComponent (PromoteError coord piece) = "Could not promote at " <> (show coord) <> " to "   <> (show piece)
    showErrorComponent (CastleError c)            = "Could not castle: " <> (show c) 
    showErrorComponent (GameError o)              = "Disallowed due to: " <> (show o)

type Parser a = Parsec ChessError String a

failWith :: ChessError -> Maybe a -> Parser a
failWith error (Nothing) = M.fancyFailure $ S.fromList [M.ErrorCustom error]
failWith error (Just a)  = return a

delimitation :: Parser ()
delimitation = void $ many (try newline <|> try spaceChar)

index :: Parser String
index = M.someTill numberChar (char '.')

file :: Parser Int
file = M.choice [(char 'a' $> 1),
                 (char 'b' $> 2), 
                 (char 'c' $> 3),
                 (char 'd' $> 4),
                 (char 'e' $> 5),
                 (char 'f' $> 6),
                 (char 'g' $> 7),
                 (char 'h' $> 8)]

promotions :: Chess.Square -> Parser Chess.Position
promotions (c, s) = M.choice [(char 'Q' $> (Chess.Pos Chess.Queen c s)),
                              (char 'R' $> (Chess.Pos Chess.Rook c s)),
                              (char 'N' $> (Chess.Pos Chess.Knight c s)),
                              (char 'B' $> (Chess.Pos Chess.Bishop c s))]

rank :: Parser Int
rank = fmap digitToInt $ numberChar

check :: Parser ()
check = void $ M.optional $ M.single '+'

mate :: Parser Bool
mate = fmap (maybe False (const True)) $ M.optional $ M.single '#'

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
capturesAt s (Chess.Enpassant _ e _) = s == e
capturesAt _ _                       = False

promotesAs :: Chess.Position -> Chess.Move -> Bool
promotesAs newPos (Chess.Promote _ newPiece enemy) = (Chess.piece newPos) == newPiece && (Chess.coord newPos) == (Chess.coord enemy)
promotesAs _ _                              = False

castlesTowards :: Chess.Dir -> Chess.Move -> Bool
castlesTowards Chess.R (Chess.Castle (_, e) _) = e == (7, 1) || e == (7, 8)
castlesTowards Chess.L (Chess.Castle (_, e) _) = e == (3, 1) || e == (3, 8)
castlesTowards _ _                       = False

captureError :: Chess.Coord -> [Chess.Move] -> ChessError
captureError _ []    = MissingMovesError
captureError c (p:_) = CaptureError c (Chess.piece $ Chess.position p)

promoteError :: Chess.Position -> [Chess.Move] -> ChessError
promoteError _ []    = MissingMovesError
promoteError p _     = PromoteError (Chess.coord p) (Chess.piece p)

advanceError :: Chess.Coord -> [Chess.Move] -> ChessError
advanceError _ []    = MissingMovesError
advanceError c (p:_) = AdvanceError c (Chess.piece $ Chess.position p)

-- there's only one piece I can capture at `x, y`
unambigousCapture :: [Chess.Move] -> Parser Chess.Move
unambigousCapture moves = do 
    _ <- char 'x'
    x <- file
    y <- rank
    failWith (captureError (x, y) moves) $ find (capturesAt (x, y)) moves

-- there's a piece which sits at file `ox` and can capture `x, y`
fileAmbigousCapture :: [Chess.Move] -> Parser Chess.Move
fileAmbigousCapture moves = do
    ox <- file
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith (captureError (x, y) moves) $ find (every [capturesAt (x, y), hasX ox]) moves 

-- there's a piece which sits at rank `oy` and can capture `x, y`
rankAmbigousCapture :: [Chess.Move] -> Parser Chess.Move
rankAmbigousCapture moves = do
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith (captureError (x, y) moves) $ find (every [capturesAt (x, y), hasY oy]) moves

-- there's a piece which sits exactly at `ox, oy` and can capture `x, y`
explicitCapture :: [Chess.Move] -> Parser Chess.Move
explicitCapture moves = do
    ox <- file
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith (captureError (x, y) moves) $ find (every [capturesAt (x, y), hasCoord (ox, oy)]) moves

-- there's only one piece which can advance to square `x, y`
unambigousAdvance :: [Chess.Move] -> Parser Chess.Move
unambigousAdvance moves = do 
    x <- file
    y <- rank
    failWith (advanceError (x, y) moves) $ find (advancesTo (x, y)) moves 

-- there's a piece which sits at file `ox` and can advance to `x, y`
fileAmbigousAdvance :: [Chess.Move] -> Parser Chess.Move
fileAmbigousAdvance moves = do
    ox <- file
    x  <- file
    y  <- rank
    failWith (advanceError (x, y) moves) $ find (every [advancesTo (x, y), hasX ox]) moves

-- there's a piece which sits at rank `oy` and can advance to `x, y`
rankAmbigousAdvance :: [Chess.Move] -> Parser Chess.Move
rankAmbigousAdvance moves = do
    oy <- rank
    x  <- file
    y  <- rank
    failWith (advanceError (x, y) moves) $ find (every [advancesTo (x, y), hasY oy]) moves

-- there's a piece which sits exactly at `ox, oy` and can advance to `x, y`
explicitAdvance :: [Chess.Move] -> Parser Chess.Move
explicitAdvance moves = do
    ox <- file
    oy <- rank
    x  <- file
    y  <- rank
    failWith (advanceError (x, y) moves) $ find (every [advancesTo (x, y), hasCoord (ox, oy)]) moves

advance :: [Chess.Move] -> Parser Chess.Move
advance moves = M.choice [try $ unambigousAdvance moves,
                          try $ fileAmbigousAdvance moves,
                          try $ rankAmbigousAdvance moves,
                          try $ explicitAdvance moves] 

capture :: [Chess.Move] ->  Parser Chess.Move
capture moves = M.choice [try $ unambigousCapture moves,
                          try $ fileAmbigousCapture moves,
                          try $ rankAmbigousCapture moves,
                          try $ explicitCapture moves]

captureOrAdvance :: [Chess.Move] -> Parser Chess.Move
captureOrAdvance moves = M.choice [capture moves, advance moves]  

-- there's a pawn at file `ox` that can capture `x, y`
capturePawn :: [Chess.Move] -> Parser Chess.Move
capturePawn moves = fileAmbigousCapture moves

-- there's a pawn at file `ox` that can advnace to `x, y`
advancePawn :: [Chess.Move] -> Parser Chess.Move
advancePawn moves = unambigousAdvance moves 

-- there's a pawn at promoting position `x, y -+ 1` and can promote at `x, y`
promotePawn :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
promotePawn colour moves = do
        x <- file
        y <- rank
        _ <- char '='
        p <- promotions (colour, (x, y))
        let coord  = pick colour (x, y)
        failWith (promoteError p moves) $ find (promotesAs p) moves
    where pick Chess.W (x, y) = (x, y - 1)
          pick Chess.B (x, y) = (x, y + 1) 

-- there's a pawn at a promoting position `ox, y +- 1` and can take left and promote at `x, y`
capturePromotePawn :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
capturePromotePawn colour moves = do
        ox <- file
        _  <- char 'x'
        x  <- file
        y  <- rank
        _  <- char '='
        p  <- promotions (colour, (x, y))
        let coord = pick colour (ox, y)
        failWith (promoteError p moves) $ find (promotesAs p) moves
    where pick Chess.W (x, y) = (x, y - 1)
          pick Chess.B (x, y) = (x, y + 1)

castle :: Chess.Castles -> [Chess.Move] -> Parser Chess.Move
castle Chess.Long  = failWith (CastleError Chess.Long) . find (castlesTowards Chess.L)
castle Chess.Short = failWith (CastleError Chess.Short) . find (castlesTowards Chess.R)
castle _           = const $ failWith MissingMovesError Nothing  

-- there's an ordering problem here aswell
-- An unambigous advance may be eagerly interpreted as a promotion in certain scenarios
-- Ex: d1=Q => `d1` by itself is a valid unambigous advance.
-- We thus have to start with checking promotions and then the rest.. 
pawn :: Chess.Board -> Parser Chess.Move
pawn board = M.choice [try $ promotePawn colour moves,
                       try $ capturePromotePawn colour moves,
                       try $ capturePawn moves,
                       try $ advancePawn moves]
        where moves  = Chess.movesPiece board (Chess.Pawn, colour)
              colour = Chess.player board 
                

rook :: Chess.Board -> Parser Chess.Move
rook board = char 'R' >> (captureOrAdvance $ Chess.movesPiece board (Chess.Rook, Chess.player board))

bishop :: Chess.Board -> Parser Chess.Move
bishop board = char 'B' >> (captureOrAdvance $ Chess.movesPiece board (Chess.Bishop, Chess.player board))

knight :: Chess.Board -> Parser Chess.Move
knight board = char 'N' >> (captureOrAdvance $ Chess.movesPiece board (Chess.Knight, Chess.player board))

queen :: Chess.Board -> Parser Chess.Move
queen board = char 'Q' >> (captureOrAdvance $ Chess.movesPiece board (Chess.Queen, Chess.player board))

-- there's an ordering problem here due to `string`
-- if `O-O` comes before `O-O-O`, `string` will catch a long castle with it
king :: Chess.Board -> Parser Chess.Move
king board = M.choice [try $ char 'K'       >> (captureOrAdvance moves),
                       try $ string "O-O-O" >> (castle Chess.Long moves),
                       try $ string "O-O"   >> (castle Chess.Short moves)]
    where moves  = Chess.movesPiece board (Chess.King, Chess.player board)

move :: Chess.Board -> Parser Chess.Move
move board = M.choice [try $ pawn board,
                       try $ rook board,
                       try $ bishop board,
                       try $ knight board,
                       try $ queen board,
                       try $ king board]

applied :: Chess.Move -> Chess.Board -> Parser Chess.Board
applied move board = case (Chess.perform board move) of (Right board') -> return board'
                                                        (Left outcome) -> failWith (GameError outcome) Nothing

moveParser :: Chess.Board -> Parser Chess.Move
moveParser board = do
    _ <- delimitation
    m <- move board
    _ <- delimitation
    _ <- applied m board
    return m