module PGN.Internal where

import qualified Chess.Internal as Chess
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

data ChessError = CaptureError Chess.Position |
                  AdvanceError Chess.Position |
                  PromoteError Chess.Position |
                  CastleError  Chess.Castles  | 
                  GameError    Chess.Outcome  |
                  MissingMovesError
                  deriving (Eq, Show, Ord)

type ParseError = M.ParseErrorBundle String ChessError

instance M.ShowErrorComponent ChessError where
    showErrorComponent (CaptureError p) = "Could not capture with: " <> (show p)
    showErrorComponent (AdvanceError p) = "Could not advance with: " <> (show p)
    showErrorComponent (PromoteError p) = "Could not promote to: " <> (show p)
    showErrorComponent (CastleError  c) = "Could not castle: " <> (show c) 
    showErrorComponent (GameError o)    = "Disallowed due to: " <> (show o)

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
capturesAt s (Chess.Capture _ e)     = s == e
capturesAt s (Chess.Enpassant _ e _) = s == e
capturesAt _ _                       = False

promotesAt :: Chess.Coord -> Chess.Move -> Bool
promotesAt s (Chess.Promote _ _ e) = s == e
promotesAt _ _                     = False

castlesTowards :: Chess.Dir -> Chess.Move -> Bool
castlesTowards Chess.R (Chess.Castle (_, e) _) = e == (7, 1) || e == (7, 8)
castlesTowards Chess.L (Chess.Castle (_, e) _) = e == (3, 1) || e == (3, 8)
castlesTowards _ _                       = False
---

-- This may give me the wrong position.
commonPosition :: [Chess.Move] -> Maybe Chess.Position
commonPosition []     = Nothing
commonPosition (a:_)  = case a of (Chess.Capture p _)     -> Just p
                                  (Chess.Advance p _)     -> Just p
                                  (Chess.Promote p _ _)   -> Just p
                                  (Chess.Castle (p, _) _) -> Just p
                                  (Chess.Enpassant p _ _) -> Just p

captureError :: [Chess.Move] -> ChessError
captureError = maybe MissingMovesError CaptureError . commonPosition

promoteError :: [Chess.Move] -> ChessError
promoteError = maybe MissingMovesError PromoteError . commonPosition

advanceError :: [Chess.Move] -> ChessError
advanceError = maybe MissingMovesError AdvanceError . commonPosition

-- there's only one piece I can capture at `x, y`
unambigousCapture :: [Chess.Move] -> Parser Chess.Move
unambigousCapture moves = do 
    _ <- char 'x'
    x <- file
    y <- rank
    failWith (captureError moves) $ find (capturesAt (x, y)) moves

-- there's a piece which sits at file `ox` and can capture `x, y`
fileAmbigousCapture :: [Chess.Move] -> Parser Chess.Move
fileAmbigousCapture moves = do
    ox <- file
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith (captureError moves) $ find (every [capturesAt (x, y), hasX ox]) moves 

-- there's a piece which sits at rank `oy` and can capture `x, y`
rankAmbigousCapture :: [Chess.Move] -> Parser Chess.Move
rankAmbigousCapture moves = do
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith (captureError moves) $ find (every [capturesAt (x, y), hasY oy]) moves

-- there's a piece which sits exactly at `ox, oy` and can capture `x, y`
explicitCapture :: [Chess.Move] -> Parser Chess.Move
explicitCapture moves = do
    ox <- file
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith (captureError moves) $ find (every [capturesAt (x, y), hasCoord (ox, oy)]) moves

-- there's only one piece which can advance to square `x, y`
unambigousAdvance :: [Chess.Move] -> Parser Chess.Move
unambigousAdvance moves = do 
    x <- file
    y <- rank
    failWith (advanceError moves) $ find (advancesTo (x, y)) moves 

-- there's a piece which sits at file `ox` and can advance to `x, y`
fileAmbigousAdvance :: [Chess.Move] -> Parser Chess.Move
fileAmbigousAdvance moves = do
    ox <- file
    x  <- file
    y  <- rank
    failWith (advanceError moves) $ find (every [advancesTo (x, y), hasX ox]) moves

-- there's a piece which sits at rank `oy` and can advance to `x, y`
rankAmbigousAdvance :: [Chess.Move] -> Parser Chess.Move
rankAmbigousAdvance moves = do
    oy <- rank
    x  <- file
    y  <- rank
    failWith (advanceError moves) $ find (every [advancesTo (x, y), hasY oy]) moves

-- there's a piece which sits exactly at `ox, oy` and can advance to `x, y`
explicitAdvance :: [Chess.Move] -> Parser Chess.Move
explicitAdvance moves = do
    ox <- file
    oy <- rank
    x  <- file
    y  <- rank
    failWith (advanceError moves) $ find (every [advancesTo (x, y), hasCoord (ox, oy)]) moves

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
        failWith (promoteError moves) $ find (promotesAt (x, y)) moves
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
        failWith (promoteError moves) $ find (promotesAt (x, y)) moves
    where pick Chess.W (x, y) = (x, y - 1)
          pick Chess.B (x, y) = (x, y + 1)

castle :: Chess.Castles -> [Chess.Move] -> Parser Chess.Move
castle Chess.Long  = failWith (CastleError Chess.Long) . find (castlesTowards Chess.L)
castle Chess.Short = failWith (CastleError Chess.Short) . find (castlesTowards Chess.R)
castle _           = const $ failWith MissingMovesError Nothing  

-- there's an ordering problem here aswell
-- An unambigous advance may be eagerly interpreted as a promotion in certain scenarios
-- Ex: d1=Q => `d1` by itself is a valid unambigous promotion.
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
parseCompute game = parseGame game >>= (handle . runGame)
    where handle (Right board)  = Right board 
          handle (Left outcome) = run (failWith (GameError outcome) Nothing) ""

runGame :: Game -> Either Chess.Outcome Chess.Board
runGame = foldl proceed (Right Chess.board) . moves
    where proceed (Right board) = Chess.perform board
          proceed a             = const a

run :: (M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> Either (M.ParseErrorBundle s e) a
run p = runParser p ""

--- DEBUG ---

printResult :: (M.Stream s, M.ShowErrorComponent e, Show a) => Either (M.ParseErrorBundle s e) a -> IO ()
printResult (Left bundle)  = putStrLn $ M.errorBundlePretty bundle
printResult (Right result) = putStrLn $ show result

runPrint :: (Show a, M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> IO ()
runPrint p = printResult . run p