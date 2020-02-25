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
import Lib

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
                  MoveError Chess.Move     |
                  GameError Chess.Outcome
                  deriving (Eq, Show, Ord)

type ParseError = M.ParseErrorBundle String ChessError

instance M.ShowErrorComponent ChessError where
    showErrorComponent (CaptureError p)    = "Could not capture with: " <> (show p)
    showErrorComponent (AdvanceError p)    = "Could not advance with: " <> (show p)
    showErrorComponent (MoveError m)       = "Illegal move: " <> (show m)
    showErrorComponent (GameError o)       = "Game is in: " <> (show o)
    --showErrorComponent (PromoteError Pawn) = "Could not promote pawn"
    showErrorComponent (PromoteError p)    = "Could not promote to: " <> (show p)

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

file :: Parser Int
file = M.choice [(char 'a' $> 1),
                 (char 'b' $> 2), 
                 (char 'c' $> 3),
                 (char 'd' $> 4),
                 (char 'e' $> 5),
                 (char 'f' $> 6),
                 (char 'g' $> 7),
                 (char 'h' $> 8)]

promotions :: Chess.Pos -> Chess.Board -> Parser Chess.Piece
promotions pos board = M.choice [(char 'Q' $> (Chess.Queen colour pos)),
                                 (char 'R' $> (Chess.Rook colour pos)),
                                 (char 'N' $> (Chess.Knight colour pos)),
                                 (char 'B' $> (Chess.Bishop colour pos))]
    where colour = Chess.player board

rank :: Parser Int
rank = fmap digitToInt $ numberChar

check :: Parser ()
check = void $ M.optional $ M.single '+'

mate :: Parser ()
mate = void $ M.optional $ M.single '#'

taking :: Chess.Pos -> [Chess.Move] -> Maybe Chess.Move
taking square = find take
    where take (Chess.Take piece piece')           = (Chess.colour piece /= Chess.colour piece') && (Chess.position piece' == square)
          take (Chess.TakeEP piece piece' piece'') = (Chess.colour piece /= Chess.colour piece'') && (Chess.position piece' == square)
          take _ = False

blocking :: Chess.Pos -> [Chess.Move] -> Maybe Chess.Move
blocking square = find move
    where move (Chess.Block piece (Chess.Empty square')) = square == square'
          move (Chess.Jump piece (Chess.Empty square'))  = square == square'
          move _ = False

promoting :: Chess.Piece -> [Chess.Move] -> Maybe Chess.Move
promoting piece = find promotion
    where promotion (Chess.Promote _ piece')       = piece == piece'
          promotion (Chess.TakePromote _ _ piece') = piece == piece'
          promotion _                              = False

-- Can't I abstract over these?
advancesTo :: Chess.Coord -> Chess.Move -> Bool
advancesTo s (Advance _ e) = s == e
advanceTo _ _              = False

capturesAt :: Chess.Coord -> Chess.Move -> Bool
capturesAt s (Capture _ e) = s == e
capturesAt _ _             = False
---

-- there's only one piece I can capture at `x, y`
unambigousCapture :: (Chess.Piece -> Bool) -> [Chess.Move] -> ChessError -> Parser Chess.Move
unambigousCapture p moves error = do 
    _ <- char 'x'
    x <- file
    y <- rank
    failWith error $ find (every [p, capturesAt (x, y)]) moves

-- there's a piece which sits at file `ox` and can capture `x, y`
fileAmbigousCapture :: (Chess.Piece -> Bool) -> [Chess.Move] -> ChessError -> Parser Chess.Move
fileAmbigousCapture p moves error = do
    ox <- file
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith error $ find (every [p, capturesAt (x, y), (== ox) . fst . Chess.coord . Chess.position]) moves 

-- there's a piece which sits at rank `oy` and can capture `x, y`
rankAmbigousCapture :: (Chess.Piece -> Bool) -> [Chess.Move] -> ChessError -> Parser Chess.Move
rankAmbigousCapture p moves error = do
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith error $ find (every [p, capturesAt (x, y), (== oy) . snd . Chess.coord . Chess.position]) moves

-- there's a piece which sits exactly at `ox, oy` and can capture `x, y`
explicitCapture :: (Chess.Piece -> Bool) -> [Chess.Move] -> ChessError -> Parser Chess.Move
explicitCapture p moves error = do
    ox <- file
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    failWith error $ find (every [p, capturesAt (x, y), (== (ox, oy)) . Chess.coord . Chess.position]) moves

-- there's only one piece which can advance to square `x, y`
unambigousAdvance :: (Chess.Position -> Bool) -> [Chess.Move] -> ChessError -> Parser Chess.Move
unambigousAdvance p moves error = do 
    x <- file
    y <- rank
    failWith error $ find (every [advancesTo (x, y), p]) moves 

-- there's a piece which sits at file `ox` and can advance to `x, y`
fileAmbigousAdvance :: (Chess.Position -> Bool) -> [Chess.Move] -> ChessError -> Parser Chess.Move
fileAmbigousAdvance p moves error = do
    ox <- file
    x  <- file
    y  <- rank
    failWith error $ find (every [p, advancesTo (x, y), (== ox) . fst . Chess.coord . Chess.position]) moves -- I have to extract the position from every move

-- there's a piece which sits at rank `oy` and can advance to `x, y`
rankAmbigousAdvance :: (Chess.Position -> Bool) -> [Chess.Move] -> ChessError -> Parser Chess.Move
rankAmbigousAdvance p moves error = do
    oy <- rank
    x  <- file
    y  <- rank
    failWith error $ find (every [p, advancesTo (x, y) , (== oy) . snd . Chess.coord . Chess.position]) moves

-- there's a piece which sits exactly at `ox, oy` and can advance to `x, y`
explicitAdvance :: (Chess.Piece -> Bool) ->  [Chess.Move] -> ChessError -> Parser Chess.Move
explicitAdvance p moves error = do
    ox <- file
    oy <- rank
    x  <- file
    y  <- rank
    let pieceAt pos piece = p piece && Chess.position piece == pos
    failWith error $ find (every [p, advancesTo (x, y), (== (ox, oy)) . Chess.coord . Chess.position]) moves

advance :: (Chess.Position -> Bool) -> [Chess.Move] -> ChessError -> Parser Chess.Move
advance p moves err = M.choice [try $ unambigousAdvance p moves err,
                                try $ fileAmbigousAdvance p moves err,
                                try $ rankAmbigousAdvance p moves err,
                                try $ explicitAdvance p moves err] 

capture :: (Chess.Position -> Bool) -> [Chess.Move] -> ChessError ->  Parser Chess.Move
capture p moves err = M.choice [try $ unambigousCapture p moves err,
                                try $ fileAmbigousCapture p moves err,
                                try $ rankAmbigousCapture p moves err,
                                try $ explicitCapture p moves err]

captureOrAdvance :: Chess.Piece -> (Chess.Piece -> Bool) -> [Chess.Move] -> Parser Chess.Move
captureOrAdvance piece p moves = capture p moves (CaptureError piece) <|> advance p moves (AdvanceError piece)  

-- there's a pawn at file `ox` that can capture `x, y`
capturePawn :: Chess.Board -> [Chess.Move] -> Parser Chess.Move
capturePawn board moves = fileAmbigousCapture (every [((== Chess.player board) . Chess.colour . Chess.position), ((== Chess.Pawn) . Chess.piece . Chess.position])) moves (CaptureError Chess.Pawn)

-- there's a pawn at file `ox` that can advnace to `x, y`
advancePawn :: Chess.Board -> [Chess.Move] -> Parser Chess.Move
advancePawn board moves = fileAmbigousAdvance (every [((== Chess.player board) . Chess.colour . Chess.position), ((== Chess.Pawn) . Chess.piece . Chess.position])) moves (AdvanceError Chess.Pawn) 

promotePawn :: Chess.Board -> Parser Chess.Move
promotePawn board = do
            x <- file
            y <- rank
            _ <- char '='
            p <- promotions (x, y) board
            let colour = Chess.player board
                pos    = case colour of Chess.W -> (x, y - 1)
                                        Chess.B -> (x, y + 1)
                pawn (Chess.Pawn c pp) = (pp == pos) && c == colour
                pawn _                 = False
            failWith (PromoteError Pawn) $ promoting p $ S.toList $ Chess.movesFor pawn board 

takePromotePawn :: Chess.Board -> Parser Chess.Move
takePromotePawn board = do
            x  <- file
            _  <- char 'x'
            ox <- file
            oy <- rank
            _  <- char '='
            p  <- promotions (ox, oy) board
            let colour = Chess.player board
                pos    = case colour of Chess.W -> (x, oy - 1)
                                        Chess.B -> (x, oy + 1)
                pawn (Chess.Pawn c pp) = (pp == pos) && (c == colour)
                pawn _                 = False
            failWith (PromoteError Pawn) $ promoting p $ S.toList $ Chess.movesFor pawn board

pawn :: Chess.Board -> Parser Chess.Move
pawn board = (try $ takePromotePawn board) <|> (try $ takePawn board) <|> (try $ promotePawn board) <|> (try $ blockPawn board)   

rook :: Chess.Board -> Parser Chess.Move
rook board = char 'R' >> takeOrBlock Rook board isRook
    where isRook (Chess.Rook player _) = player == Chess.player board
          isRook _                     = False

bishop :: Chess.Board -> Parser Chess.Move
bishop board = char 'B' >> takeOrBlock Bishop board isBishop
    where isBishop (Chess.Bishop player _) = player == Chess.player board  
          isBishop _                       = False

knight :: Chess.Board -> Parser Chess.Move
knight board = char 'N' >> takeOrBlock Knight board isKnight
    where isKnight (Chess.Knight player _) = player == Chess.player board
          isKnight _                       = False

queen :: Chess.Board -> Parser Chess.Move
queen board = char 'Q' >> takeOrBlock Queen board isQueen
    where isQueen (Chess.Queen player _) = player == Chess.player board 
          isQueen _                      = False

king :: Chess.Board -> Parser Chess.Move
king board = char 'K' >> takeOrBlock King board isKing
    where isKing (Chess.King player _) = player == Chess.player board
          isKing _                     = False

-- verify these again
kingCastle :: Chess.Board -> Parser Chess.Move
kingCastle board = string "O-O" $> (castleBy $ Chess.player board)
    where castleBy W = Chess.Castle (Chess.Pos Chess.King W (5, 1), (7, 1)) 
                                    (Chess.Pos Chess.Rook W (8, 1), (6, 1))
          castleBy B = Chess.Castle (Chess.Pos Chess.King B (5, 8), (7, 8))
                                    (Chess.Pos Chess.Rook B (8, 8), (6, 8))

queenCastle :: Chess.Board -> Parser Chess.Move
queenCastle board = string "O-O-O" $> (castleBy $ Chess.player board)
    where castleBy W = Chess.Castle (Chess.Pos Chess.King W (5, 1), (3, 1)) 
                                    (Chess.Pos Chess.Rook W (1, 1), (4, 1))
          castleBy B = Chess.Castle (Chess.Pos Chess.King B (5, 8), (3, 8))
                                    (Chess.Pos Chess.Rook B (1, 8), (4, 8))

castle :: Chess.Board -> Parser Chess.Move
castle board = (try $ queenCastle board) <|> (try $ kingCastle board)

move :: Chess.Board -> Parser Chess.Move
move board = (try $ pawn board)   <|>
             (try $ king board)   <|>
             (try $ rook board)   <|>
             (try $ bishop board) <|> 
             (try $ knight board) <|>
             (try $ queen board)  <|>
             (try $ castle board)

applied :: Chess.Move -> Chess.Board -> Parser Chess.Board
applied move = either fail return . Chess.move move
    where fail Chess.Illegal = failWith (MoveError move) Nothing
          fail outcome       = failWith (GameError outcome) Nothing

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
parseCompute game = (parseGame game) >>= (either (Left . convert) (Right) . runGame)
    where convert outcome = either id (const $ error "This never should succeed") $ run (failWith (GameError outcome) Nothing) ""

runGame :: Game -> Either Chess.Outcome Chess.Board
runGame = foldl (\b m -> b >>= (Chess.move m)) (pure Chess.board) . moves

run :: (M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> Either (M.ParseErrorBundle s e) a
run p = runParser p ""

--- DEBUG ---

printResult :: (M.Stream s, M.ShowErrorComponent e, Show a) => Either (M.ParseErrorBundle s e) a -> IO ()
printResult (Left bundle)  = putStrLn $ M.errorBundlePretty bundle
printResult (Right result) = putStrLn $ show result

runPrint :: (Show a, M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> IO ()
runPrint p = printResult . run p