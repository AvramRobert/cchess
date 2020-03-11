module PGN.Internal2 where

import qualified Chess.Internal2 as Chess
import qualified Text.Megaparsec as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Monoid (Monoid)
import Data.ByteString.Lazy (ByteString)
import Text.Megaparsec (Parsec, (<|>), runParser, try, many)
import Text.Megaparsec.Char (char, string, spaceChar, numberChar, asciiChar, newline)
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Control.Monad (void)
import Data.Functor (($>))
import Data.List (find)
import System.IO.Unsafe (unsafePerformIO)
import Lib.Coll

data ChessResult = WhiteWin | BlackWin | Draw deriving (Show)

data Game = Game {
                  event       :: String, 
                  site        :: String, 
                  date        :: String,
                  gameRound   :: String,
                  whitePlayer :: String,
                  blackPlayer :: String,
                  endResult   :: ChessResult,
                  -- These upper 7 are mandatory, the lower 4 aren't
                  whiteElo    :: Maybe Int,
                  blackElo    :: Maybe Int, 
                  eco         :: Maybe String,
                  eventDate   :: Maybe String,
                  moves       :: [Chess.Move] } deriving (Show)

data ChessError = CaptureError Chess.Piece |
                  AdvanceError Chess.Piece |
                  PromoteError Chess.Piece |
                  CastleError  Chess.Dir   | 
                  GameError    Chess.Outcome
                  deriving (Eq, Show, Ord)

type ParseError = M.ParseErrorBundle String ChessError

-- This should probably also show the colour
instance M.ShowErrorComponent ChessError where
    showErrorComponent (CaptureError p)      = "Could not capture with: " <> (show p)
    showErrorComponent (AdvanceError p)      = "Could not advance with: " <> (show p)
    showErrorComponent (PromoteError p)      = "Could not promote to: " <> (show p)
    showErrorComponent (CastleError Chess.R) = "Could not castle kingside"
    showErrorComponent (CastleError Chess.L) = "Could not castle queenside"
    showErrorComponent (GameError o)         = "Disallowed due to: " <> (show o)

type Parser a = Parsec ChessError String a

data Turn = End | One Chess.Move | Two (Chess.Move, Chess.Move) deriving (Show, Eq)

failWith :: ChessError -> Maybe a -> Parser a
failWith error (Nothing) = M.fancyFailure $ S.fromList [M.ErrorCustom error]
failWith error (Just a)  = return a

merge :: [ByteString] -> String
merge = foldl combine ""
    where combine s b = s <> C.unpack b

extractGame :: [ByteString] -> (String, [ByteString])
extractGame lines = (merge (header <> gameLines), remaining)
        where header    = takeWhile headline lines
              headless  = dropWhile headline lines
              gameLines = takeWhile (not . headline) headless
              remaining = dropWhile (not . headline) headless
              headline string = (not $ C.null string) && C.head string == '['

-- apparently there's no sane way to consume any arbitrary ascii characters between two quotes, without forcing the user to lookAhead for the quote
-- between (char '"') (char '"') (many asciiChar) will fail because (many asciiChar) will consume the last '"' thus `between` will never work
-- so it has to be this: between (char '"') (char '"') (manyTill asciiChar (lookAhead (char '"')))
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

headerParser :: Parser Game
headerParser = do
    event   <- headline "Event" characters
    site    <- headline "Site" characters
    date    <- headline "Date" characters
    ground  <- headline "Round" characters
    whitep  <- headline "White" characters
    blackp  <- headline "Black" characters
    outcome <- headline "Result" result
    whitee  <- headline "WhiteElo" numbers
    blacke  <- headline "BlackElo" numbers
    eco     <- M.optional $ headline "ECO" characters
    eventd  <- M.optional $ headline "EventDate" characters
    return $ Game  {event       = event,
                    site        = site,
                    date        = date,
                    gameRound   = ground,
                    whitePlayer = whitep,
                    blackPlayer = blackp,
                    endResult   = outcome,
                    whiteElo    = whitee,
                    blackElo    = blacke,
                    eco         = eco,
                    eventDate   = eventd,
                    moves       = []}
    where characters          = M.manyTill asciiChar (M.lookAhead $ char '"')
          numbers             = fmap (>>= readMaybe) $ M.optional $ many numberChar

delimitation :: Parser ()
delimitation = void $ many (try newline <|> try spaceChar)

index :: Parser String
index = M.someTill numberChar (char '.')

file :: Parser Integer
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

rank :: Parser Integer
rank = fmap (toInteger . digitToInt) $ numberChar

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

hasX :: Integer -> Chess.Move -> Bool
hasX x = (== x) . fst . Chess.coord . Chess.position

hasY :: Integer -> Chess.Move -> Bool
hasY y = (== y) . snd . Chess.coord . Chess.position

-- Can't I abstract over these?
advancesTo :: Chess.Coord -> Chess.Move -> Bool
advancesTo s (Chess.Advance _ e) = s == e
advancesTo _ _                    = False

capturesAt :: Chess.Coord -> Chess.Move -> Bool
capturesAt s (Chess.Capture _ e) = s == e
capturesAt _ _                   = False

promotesAt :: Chess.Coord -> Chess.Move -> Bool
promotesAt s (Chess.Promote _ _ e) = s == e
promotesAt _ _                     = False

castlesTowards :: Chess.Dir -> Chess.Move -> Bool
castlesTowards Chess.R (Chess.Castle (_, e) _) = e == (7, 1) || e == (7, 8)
castlesTowards Chess.L (Chess.Castle (_, e) _) = e == (3, 1) || e == (3, 8)
castlesTowards _ _                       = False
---

-- there's only one piece I can capture at `x, y`
unambigousCapture :: (Chess.Piece, Chess.Colour) -> [Chess.Move] -> ChessError -> Parser Chess.Move
unambigousCapture (p, c) moves error = do 
    _ <- char 'x'
    x <- file
    y <- rank
    failWith error $ find (every [capturesAt (x, y), hasColour c, hasPiece p]) moves

-- there's a piece which sits at file `ox` and can capture `x, y`
fileAmbigousCapture :: (Chess.Piece, Chess.Colour) -> [Chess.Move] -> ChessError -> Parser Chess.Move
fileAmbigousCapture (p, c) moves error = do
    ox <- file
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith error $ find (every [capturesAt (x, y), hasColour c, hasPiece p, hasX ox]) moves 

-- there's a piece which sits at rank `oy` and can capture `x, y`
rankAmbigousCapture :: (Chess.Piece, Chess.Colour) -> [Chess.Move] -> ChessError -> Parser Chess.Move
rankAmbigousCapture (p, c) moves error = do
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith error $ find (every [capturesAt (x, y), hasColour c, hasPiece p, hasY oy]) moves

-- there's a piece which sits exactly at `ox, oy` and can capture `x, y`
explicitCapture :: (Chess.Piece, Chess.Colour) -> [Chess.Move] -> ChessError -> Parser Chess.Move
explicitCapture (p, c) moves error = do
    ox <- file
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith error $ find (every [capturesAt (x, y), hasColour c, hasPiece p, hasCoord (ox, oy)]) moves

-- there's only one piece which can advance to square `x, y`
unambigousAdvance :: (Chess.Piece, Chess.Colour) -> [Chess.Move] -> ChessError -> Parser Chess.Move
unambigousAdvance (p, c) moves error = do 
    x <- file
    y <- rank
    failWith error $ find (every [advancesTo (x, y), hasColour c, hasPiece p]) moves 

-- there's a piece which sits at file `ox` and can advance to `x, y`
fileAmbigousAdvance :: (Chess.Piece, Chess.Colour) -> [Chess.Move] -> ChessError -> Parser Chess.Move
fileAmbigousAdvance (p, c) moves error = do
    ox <- file
    x  <- file
    y  <- rank
    failWith error $ find (every [advancesTo (x, y), hasColour c, hasPiece p, hasX ox]) moves -- I have to extract the position from every move

-- there's a piece which sits at rank `oy` and can advance to `x, y`
rankAmbigousAdvance :: (Chess.Piece, Chess.Colour) -> [Chess.Move] -> ChessError -> Parser Chess.Move
rankAmbigousAdvance (p, c) moves error = do
    oy <- rank
    x  <- file
    y  <- rank
    failWith error $ find (every [advancesTo (x, y), hasColour c, hasPiece p, hasY oy]) moves

-- I think I can remove every predicate and replace them with a (Piece, Colour) tuple
-- there's a piece which sits exactly at `ox, oy` and can advance to `x, y`
explicitAdvance :: (Chess.Piece, Chess.Colour) -> [Chess.Move] -> ChessError -> Parser Chess.Move
explicitAdvance (p, c) moves error = do
    ox <- file
    oy <- rank
    x  <- file
    y  <- rank
    failWith error $ find (every [advancesTo (x, y), hasColour c, hasPiece p, hasCoord (ox, oy)]) moves

advance :: (Chess.Piece, Chess.Colour) -> [Chess.Move] -> ChessError -> Parser Chess.Move
advance p moves err = M.choice [try $ unambigousAdvance p moves err,
                                try $ fileAmbigousAdvance p moves err,
                                try $ rankAmbigousAdvance p moves err,
                                try $ explicitAdvance p moves err] 

capture :: (Chess.Piece, Chess.Colour) -> [Chess.Move] -> ChessError ->  Parser Chess.Move
capture p moves err  = M.choice [try $ unambigousCapture p moves err,
                                 try $ fileAmbigousCapture p moves err,
                                 try $ rankAmbigousCapture p moves err,
                                 try $ explicitCapture p moves err]

captureOrAdvance :: (Chess.Piece, Chess.Colour) -> [Chess.Move] -> Parser Chess.Move
captureOrAdvance pair moves = M.choice [capture pair moves (CaptureError $ fst pair),
                                        advance pair moves (AdvanceError $ fst pair)]  

-- there's a pawn at file `ox` that can capture `x, y`
capturePawn :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
capturePawn colour moves = fileAmbigousCapture (Chess.Pawn, colour) moves (CaptureError Chess.Pawn)

-- there's a pawn at file `ox` that can advnace to `x, y`
advancePawn :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
advancePawn colour moves = unambigousAdvance (Chess.Pawn, colour) moves (AdvanceError Chess.Pawn) 

-- there's a pawn at promoting position `x, y -+ 1` and can promote at `x, y`
promotePawn :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
promotePawn colour moves = do
        x <- file
        y <- rank
        _ <- char '='
        p <- promotions (colour, (x, y))
        let coord  = pick colour (x, y)
        failWith (PromoteError Chess.Pawn) $ find (every [promotesAt (x, y), hasCoord coord, hasColour colour]) moves
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
        failWith (PromoteError Chess.Pawn) $ find (every [promotesAt (x, y), hasCoord coord, hasColour colour]) moves
    where pick Chess.W (x, y) = (x, y - 1)
          pick Chess.B (x, y) = (x, y + 1)

-- takePromotePawn :: Chess.Board -> Parser Chess.Move
-- takePromotePawn board = do
--             x  <- file
--             _  <- char 'x'
--             ox <- file
--             oy <- rank
--             _  <- char '='
--             p  <- promotions (ox, oy) board
--             let colour = Chess.player board
--                 pos    = case colour of Chess.W -> (x, oy - 1)
--                                         Chess.B -> (x, oy + 1)
--                 pawn (Chess.Pawn c pp) = (pp == pos) && (c == colour)
--                 pawn _                 = False
--             failWith (PromoteError Pawn) $ promoting p $ S.toList $ Chess.movesFor pawn board

-- theoretically, after this point, I don't need a Chess.Board for every sub-predicate
pawn :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
pawn colour moves = M.choice [try $ capturePawn colour moves,
                              --try $ capturePromotePawn colour moves,
                              --try $ promotePawn colour moves,
                              try $ advancePawn colour moves]   

rook :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
rook colour moves = char 'R' >> captureOrAdvance (Chess.Rook, colour) moves

bishop :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
bishop colour moves = char 'B' >> captureOrAdvance (Chess.Bishop, colour) moves

knight :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
knight colour moves = char 'N' >> captureOrAdvance (Chess.Knight, colour) moves

queen :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
queen colour moves = char 'Q' >> captureOrAdvance (Chess.Queen, colour) moves

king :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
king colour moves = char 'K' >> captureOrAdvance (Chess.King, colour) moves

kingCastle :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
kingCastle colour moves = string "O-O" >> (failWith (CastleError Chess.R) $ find (every [castlesTowards Chess.R, hasColour colour]) moves)

queenCastle :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
queenCastle colour moves = string "O-O-O" >> (failWith (CastleError Chess.L) $ find (every [castlesTowards Chess.L, hasColour colour]) moves)

castle :: Chess.Colour -> [Chess.Move] -> Parser Chess.Move
castle colour moves = M.choice [try $ queenCastle colour moves,
                                try $ kingCastle colour moves]

move :: Chess.Board -> Parser Chess.Move
move board = M.choice [try $ pawn colour moves,
                       try $ king colour moves,
                       try $ rook colour moves,
                       try $ bishop colour moves,
                       try $ knight colour moves,
                       try $ queen colour moves,
                       try $ castle colour moves]
    where moves  = Chess.allMoves board
          colour = Chess.player board

applied :: Chess.Move -> Chess.Board -> Parser Chess.Board
applied move board = if (outcome == Chess.Continue)
                     then return board' 
                     else failWith (GameError outcome) Nothing
    where (outcome, board') = Chess.perform board move

-- if no mate occurs, but the game stops, then depending on the result, the outcome may be a forfeit, draw, stalemate or checkmate
-- this however should be part of the in-game parser
result :: Parser ChessResult
result = M.choice [(try $ string "1-0") $> WhiteWin,
                   (try $ string "0-1") $> BlackWin,
                   (string "1/2-1/2")   $> Draw]

noMove :: Chess.Board -> Parser (Chess.Board, Turn)
noMove board = result $> (board, End)

oneMove :: Chess.Board -> Parser (Chess.Board, Turn)
oneMove board = do
    _ <- delimitation
    _ <- index
    _ <- delimitation
    m <- move board
    b <- applied m board
    _ <- check
    _ <- mate
    _ <- delimitation
    _ <- result
    return (b, One m)

twoMove :: Chess.Board -> Parser (Chess.Board, Turn)
twoMove board = do
    _  <- delimitation
    _  <- index
    _  <- delimitation
    m  <- move board
    b  <- applied m board
    _  <- check
    _  <- mate
    _  <- delimitation
    m' <- move b
    b' <- applied m' b
    _  <- check
    _  <- mate
    _  <- delimitation
    return (b', Two (m, m'))

turn :: Chess.Board -> Parser (Chess.Board, Turn)
turn board = (try $ noMove board) <|> (try $ oneMove board) <|> (try $ twoMove board)

turnMoveParser :: Parser [Chess.Move]
turnMoveParser = turns [] Chess.board
        where turns moves board = turn board >>= (continue moves) 
              continue moves (board, Two (m, m')) = turns (m' : m : moves) board
              continue moves (board, One m) = continue (m : moves) (board, End)
              continue moves (board, End) = return $ reverse moves 

moveParser :: Chess.Board -> Parser Chess.Move
moveParser board = do
    _ <- delimitation
    m <- move board
    _ <- delimitation
    _ <- applied m board
    return m

gameParser :: Parser Game
gameParser = do
        _      <- delimitation 
        meta   <- headerParser
        _      <- delimitation
        gmoves <- turnMoveParser
        return meta { moves = gmoves }

splitGames :: ByteString -> [String]
splitGames = accumulate . C.lines
    where accumulate [] = []
          accumulate ls = let (game, remaining) = extractGame ls
                          in game : (accumulate remaining)

fromPGNFile' :: String -> IO [String]
fromPGNFile' = fmap splitGames . B.readFile

fromString' :: String -> [String]
fromString' = splitGames . C.pack

fromPGNFile :: String -> IO (Either ParseError [Game])
fromPGNFile = fmap (sequence . fmap (run gameParser)) . fromPGNFile'

fromString :: String -> Either ParseError [Game]
fromString = sequence . fmap (run gameParser) . splitGames . C.pack

parseGame :: String -> Either ParseError Game
parseGame = run gameParser

parseMove ::  String -> Chess.Board -> Either ParseError Chess.Move
parseMove move board = run (moveParser board) move 

parseCompute :: String -> Either ParseError Chess.Board
parseCompute = fmap (snd . runGame) . parseGame

runGame :: Game -> (Chess.Outcome, Chess.Board)
runGame = foldl proceed (Chess.Continue, Chess.board) . moves
    where proceed (Chess.Continue, board) = Chess.perform board
          proceed a                       = const a

run :: (M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> Either (M.ParseErrorBundle s e) a
run p = runParser p ""

--- DEBUG ---

printResult :: (M.Stream s, M.ShowErrorComponent e, Show a) => Either (M.ParseErrorBundle s e) a -> IO ()
printResult (Left bundle)  = putStrLn $ M.errorBundlePretty bundle
printResult (Right result) = putStrLn $ show result

runPrint :: (Show a, M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> IO ()
runPrint p = printResult . run p