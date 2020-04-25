module PGN.Internal where

import qualified Chess.Internal as Chess
import qualified Text.Megaparsec as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.List.NonEmpty as NL
import qualified Chess.Meta as G
import Data.Monoid (Monoid)
import Data.ByteString.Lazy (ByteString)
import Text.Megaparsec (Parsec, (<|>), runParser, try, many)
import Text.Megaparsec.Char (char, char', string, string', spaceChar, numberChar, asciiChar, newline)
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Control.Monad (void, forM)
import Data.Functor (($>))
import Data.List (find, sort)
import System.IO.Unsafe (unsafePerformIO)
import Lib.Coll
import Chess.Display

data ChessError = CaptureError Chess.Coord Chess.Figure |
                  AdvanceError Chess.Coord Chess.Figure |
                  PromoteError Chess.Coord Chess.Figure | 
                  CastleError  Chess.Castles  | 
                  GameError    Chess.Outcome  |
                  MissingMovesError
                  deriving (Eq, Show, Ord)

type ParseError = M.ParseErrorBundle String ChessError

-- Move this to `Display`?
instance M.ShowErrorComponent ChessError where
    showErrorComponent (CaptureError coord piece) = "Could not capture at " <> (show coord) <> " with " <> (show piece)
    showErrorComponent (AdvanceError coord piece) = "Could not advance to " <> (show coord) <> " with " <> (show piece)
    showErrorComponent (PromoteError coord piece) = "Could not promote at " <> (show coord) <> " to "   <> (show piece)
    showErrorComponent (CastleError c)            = "Could not castle: " <> (show c) 
    showErrorComponent (GameError o)              = "Disallowed due to: " <> (show o)

type Parser a = Parsec ChessError String a

data Turn = Ended | Mated | Moved deriving (Show, Eq)

failWith :: ChessError -> Maybe a -> Parser a
failWith error (Nothing) = M.fancyFailure $ S.fromList [M.ErrorCustom error]
failWith error (Just a)  = return a

chessError :: ParseError -> Maybe ChessError
chessError error = case (NL.head $ M.bundleErrors error) of
        (M.FancyError _ errorSet) -> Just $ strip $ head $ S.toList errorSet
        (_)                       -> Nothing
    where strip (M.ErrorCustom e) = e

extractGame :: [ByteString] -> (String, [ByteString])
extractGame lines = (C.unpack $ C.unlines (header <> gameLines), remaining)
        where header    = takeWhile headline lines
              headless  = dropWhile headline lines
              gameLines = takeWhile (not . headline) headless
              remaining = dropWhile (not . headline) headless
              headline  = every [not . C.null, (== '[') . C.head]

-- apparently there's no sane way to consume any arbitrary ascii characters between two quotes, without forcing the user to lookAhead for the quote
-- between (char '"') (char '"') (many asciiChar) will fail because (many asciiChar) will consume the last '"' thus `between` will never work
-- so it has to be this: between (char '"') (char '"') (manyTill asciiChar (lookAhead (char '"')))
characters :: Parser String
characters = M.manyTill asciiChar (M.lookAhead $ char '"')

numbers :: Parser (Maybe Int)
numbers = fmap (>>= readMaybe) $ M.optional $ many numberChar

tagline :: Parser String -> Parser a -> Parser a
tagline title content = do
        _ <- delimitation
        _ <- char '['
        _ <- title
        _ <- delimitation
        a <- M.between (char '"') (char '"') content
        _ <- char ']'
        _ <- delimitation
        return a

tagParsers :: [Parser G.Tag]
tagParsers = [try $ extract characters "event" G.Event, 
              try $ extract characters "site" G.Site, 
              try $ extract characters "date" G.Date,
              try $ extract characters "round" G.Round,
              try $ extract characters "white" G.White,
              try $ extract characters "black" G.Black,
              try $ extract result     "result" G.Result, 
              ignore]
    where extract what title to = fmap to $ tagline (string' title) what
          ignore                = tagline characters characters $> G.Ignored

-- Some PGN files exports don't abide to the format rules..
-- I have to parse without ordering and them order them properly afterwards
tagsParser :: Parser [G.Tag]
tagsParser = fmap sort $ (M.optional (M.choice tagParsers) >>= process)
    where process (Just G.Ignored)  = tagsParser
          process (Just h)        = fmap (\hs -> h:hs) tagsParser
          process (Nothing)       = return []

headline :: String -> Parser a -> Parser a
headline header p = do
        _ <- delimitation
        _ <- char '['
        _ <- string header
        _ <- delimitation
        a <- M.between (char '"') (char '"') p
        _ <- char ']'
        _ <- delimitation
        return a

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
promotions (c, s) = M.choice [(char' 'Q' $> (Chess.Pos Chess.Queen c s)),
                              (char' 'R' $> (Chess.Pos Chess.Rook c s)),
                              (char' 'N' $> (Chess.Pos Chess.Knight c s)),
                              (char' 'B' $> (Chess.Pos Chess.Bishop c s))]

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
captureError c (m:_) = CaptureError c (Chess.figure $ Chess.position m)

promoteError :: Chess.Position -> [Chess.Move] -> ChessError
promoteError _ []    = MissingMovesError
promoteError p _     = PromoteError (Chess.coord p) (Chess.figure p)

advanceError :: Chess.Coord -> [Chess.Move] -> ChessError
advanceError _ []    = MissingMovesError
advanceError c (m:_) = AdvanceError c $ (Chess.figure $ Chess.position m)

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
rook board = char' 'R' >> (captureOrAdvance $ Chess.movesPiece board (Chess.Rook, Chess.player board))

bishop :: Chess.Board -> Parser Chess.Move
bishop board = char' 'B' >> (captureOrAdvance $ Chess.movesPiece board (Chess.Bishop, Chess.player board))

knight :: Chess.Board -> Parser Chess.Move
knight board = char' 'N' >> (captureOrAdvance $ Chess.movesPiece board (Chess.Knight, Chess.player board))

queen :: Chess.Board -> Parser Chess.Move
queen board = char' 'Q' >> (captureOrAdvance $ Chess.movesPiece board (Chess.Queen, Chess.player board))

-- there's an ordering problem here due to `string`
-- if `O-O` comes before `O-O-O`, `string` will catch a long castle with it
king :: Chess.Board -> Parser Chess.Move
king board = M.choice [try $ char' 'K'       >> (captureOrAdvance moves),
                       try $ string' "O-O-O" >> (castle Chess.Long moves),
                       try $ string' "O-O"   >> (castle Chess.Short moves)]
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

result :: Parser G.GameResult
result = M.choice [(try $ string "1-0")     $> (G.Win Chess.W),
                   (try $ string "0-1")     $> (G.Win Chess.B),
                   (try $ string "1/2-1/2") $> G.Draw,
                   (try $ string "*")       $> G.Other]

moveParser :: Chess.Board -> Parser Chess.Move
moveParser board = do
    _ <- delimitation
    m <- move board
    _ <- delimitation
    return m

appliedTurnParser :: Chess.Board -> Parser (Chess.Move, Chess.Board, Turn)
appliedTurnParser board = do
    m <- moveParser board
    b <- applied m board
    _ <- check
    e <- mate
    _ <- delimitation
    o <- M.optional $ M.lookAhead result
    return $ case o of (Just _) | e -> (m, b, Mated)
                       (Just _)     -> (m, b, Ended)
                       (Nothing)    -> (m, b, Moved)

turnParser :: Chess.Board -> Parser (Chess.Move, Chess.Board, Turn)
turnParser board = (try firstTurn) <|> (try secondTurn)
    where firstTurn  = delimitation >> index >> appliedTurnParser board
          secondTurn = delimitation >> appliedTurnParser board  

boardParser :: Parser (Chess.Board, Turn)
boardParser = parseOn Chess.board
    where parseOn board = turnParser board >>= continue
          continue (m, b, Moved) = parseOn b
          continue (m, b, t)     = return (b, t)

endReason :: Chess.Board -> Turn -> G.GameResult -> Chess.Outcome
endReason board Mated (G.Win _) = Chess.Checkmate
endReason board _     (G.Win _) = Chess.Resignation
endReason board _     (G.Draw)  = if (Chess.stalemate board) 
                                  then Chess.Stalemate 
                                  else Chess.Draw

gameParser :: Parser G.Game
gameParser = do
    _             <- delimitation
    tags          <- tagsParser
    _             <- delimitation
    (board, turn) <- boardParser
    result        <- result
    let reason    = Just (endReason board turn result)  
    return G.Game { G.tags = tags, G.board = board }

splitGames :: ByteString -> [String]
splitGames = accumulate . C.lines
    where accumulate [] = []
          accumulate ls = let (game, remaining) = extractGame ls
                          in game : (accumulate remaining)

fromPGNFile' :: String -> IO [String]
fromPGNFile' = fmap splitGames . B.readFile

fromString' :: String -> [String]
fromString' = splitGames . C.pack

fromPGNFile :: String -> IO (Either ParseError [G.Game])
fromPGNFile = fmap (sequence . fmap (run gameParser)) . fromPGNFile'

fromString :: String -> Either ParseError [G.Game]
fromString = sequence . fmap (run gameParser) . splitGames . C.pack

parseGame :: String -> Either ParseError G.Game
parseGame = run gameParser

parseMove ::  String -> Chess.Board -> Either ParseError Chess.Move
parseMove move board = run (moveParser board) move 

parseBoard :: String -> Either ParseError Chess.Board
parseBoard = fmap G.board . parseGame 

run :: (M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> Either (M.ParseErrorBundle s e) a
run p = runParser p ""

--- DEBUG ---

printResult :: (M.Stream s, M.ShowErrorComponent e, Show a) => Either (M.ParseErrorBundle s e) a -> IO ()
printResult (Left bundle)  = putStrLn $ M.errorBundlePretty bundle
printResult (Right result) = putStrLn $ show result

runPrint :: (Show a, M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> IO ()
runPrint p = printResult . run p