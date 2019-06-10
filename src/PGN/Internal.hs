module PGN.Internal where

import qualified Chess as Chess
import qualified Text.Megaparsec as M
import qualified Data.Set as S
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Set (Set)
import Data.ByteString.Lazy (ByteString)
import Text.Megaparsec (Parsec, (<|>), runParser, try, many)
import Text.Megaparsec.Char (char, string, spaceChar, numberChar, asciiChar, newline)
import Text.Read (readMaybe)
import Data.Char (digitToInt)
import Control.Monad (void)
import Data.Functor (($>))
import Data.List (find)
import System.IO.Unsafe (unsafePerformIO)

data ChessResult = WhiteWin | BlackWin | Draw deriving (Show)

data Game = Game {
                  event       :: String, 
                  site        :: String, 
                  date        :: String,
                  gameRound   :: String,
                  whitePlayer :: String,
                  blackPlayer :: String,
                  result      :: ChessResult,
                  whiteElo    :: Int,
                  blackElo    :: Int, 
                  eco         :: String,
                  eventDate   :: String,
                  moves       :: [Chess.Move] } deriving (Show)

data ChessError = TakeError ChessPiece    |
                  BlockError ChessPiece   |
                  PromoteError ChessPiece |
                  MoveError Chess.Move    |
                  GameError Chess.Outcome |
                  EloError String
                  deriving (Eq, Show, Ord)

data ChessPiece = Pawn | Rook | Bishop | Knight | Queen | King deriving (Show, Eq, Ord)

type ParseError = M.ParseErrorBundle String ChessError

instance M.ShowErrorComponent ChessError where
    showErrorComponent (TakeError p)       = "Could not take with: " <> (show p)
    showErrorComponent (BlockError p)      = "Could not block with: " <> (show p)
    showErrorComponent (MoveError m)       = "Illegal move: " <> (show m)
    showErrorComponent (GameError o)       = "Game is in: " <> (show o)
    showErrorComponent (PromoteError Pawn) = "Could not promote pawn"
    showErrorComponent (PromoteError p)    = "Could not promote to: " <> (show p)
    showErrorComponent (EloError elos)     = "Elo is not a valid number: " <> elos

type Parser a = Parsec ChessError String a

data Turn = End | One Chess.Move | Two (Chess.Move, Chess.Move) deriving (Show, Eq)  

merge :: [ByteString] -> String
merge = foldl combine ""
    where combine s bs = s <> C.unpack bs

-- TODO: Stream this properly
extractGame :: [ByteString] -> (String, [ByteString])
extractGame lines = (merge (header <> gameLines), remaining)
        where header    = takeWhile headline lines
              headless  = dropWhile headline lines
              gameLines = takeWhile (not . headline) headless
              remaining = dropWhile (not . headline) headless
              headline string = C.head string == '['

headline :: String -> (String -> Parser a) -> Parser a
headline header f = do
        _ <- lineDelimitation
        _ <- char '['
        _ <- string header
        _ <- lineDelimitation
        _ <- char '"'
        x <- M.manyTill asciiChar (char '"')
        _ <- char ']'
        (f x)

headerParser :: Parser Game
headerParser = do
    event  <- headline "Event" return
    site   <- headline "Site" return
    date   <- headline "Date" return
    ground <- headline "Round" return
    whitep <- headline "White" return
    blackp <- headline "Black" return
    result <- headline "Result" (return . mapResult)
    whitee <- headline "WhiteElo" eloError
    blacke <- headline "BlackElo" eloError
    eco    <- headline "ECO" return
    eventd <- headline "EventDate" return
    return $ Game  {event       = event,
                    site        = site,
                    date        = date,
                    gameRound   = ground,
                    whitePlayer = whitep,
                    blackPlayer = blackp,
                    result      = result,
                    whiteElo    = whitee,
                    blackElo    = blacke,
                    eco         = eco,
                    eventDate   = eventd,
                    moves       = []}
    where mapResult "1-0"     = WhiteWin
          mapResult "0-1"     = BlackWin
          mapResult "1/2-1/2" = Draw
          eloError sint       = failWith (EloError sint) $ readMaybe sint


delimitation :: Parser ()
delimitation = void $ many spaceChar

lineDelimitation :: Parser ()
lineDelimitation = do 
        _ <- M.optional newline
        _ <- delimitation
        _ <- M.optional newline
        return ()

failWith :: ChessError -> Maybe a -> Parser a
failWith error (Nothing) = M.fancyFailure $ S.fromList [M.ErrorCustom error]
failWith error (Just a)  = return a

index :: Parser ()
index = void $ M.takeWhileP Nothing (/= '.') >> (char '.')

file :: Parser Int
file = M.choice [(char 'a' $> 1),
                 (char 'b' $> 2), 
                 (char 'c' $> 3),
                 (char 'd' $> 4),
                 (char 'e' $> 5),
                 (char 'f' $> 6),
                 (char 'g' $> 7),
                 (char 'h' $> 8)]

promotedPiece :: Chess.Pos -> Chess.Board -> Parser Chess.Piece
promotedPiece pos board = M.choice [(char 'Q' >> (convert Queen  Chess.Queen)),
                                    (char 'R' >> (convert Rook   Chess.Rook)),
                                    (char 'N' >> (convert Knight Chess.Knight)),
                                    (char 'B' >> (convert Bishop Chess.Bishop))]
    where 
        promote f piece = f (Chess.colour piece) (Chess.position piece) 
        convert piece f = failWith (PromoteError piece) $ fmap (promote f) $ Chess.lookAt pos board

rank :: Parser Int
rank = fmap digitToInt $ numberChar

check :: Parser ()
check = void $ M.takeWhileP Nothing (== '+')

mate :: Parser ()
mate = void $ M.takeWhileP Nothing (== '#')

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
    where promotion (Chess.Promote _ piece') = piece == piece' 

unambigousTake :: (Chess.Piece -> Bool) -> Chess.Board -> ChessError -> Parser Chess.Move
unambigousTake p board error = do 
    _ <- char 'x'
    x <- file
    y <- rank
    failWith error $ taking (x, y) $ S.toList $ Chess.movesFor p board

fileAmbigousTake :: (Chess.Piece -> Bool) -> Chess.Board -> ChessError -> Parser Chess.Move
fileAmbigousTake p board error = do
    ox <- file
    _  <- char 'x'
    x  <- file
    y  <- rank
    let fileIs x piece = p piece && (fst $ Chess.position piece) == x
    failWith error $ taking (x, y) $ S.toList $ Chess.movesFor (fileIs ox) board 

rankAmbigousTake :: (Chess.Piece -> Bool) -> Chess.Board -> ChessError -> Parser Chess.Move
rankAmbigousTake p board error = do
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    let rankIs y piece = p piece && (snd $ Chess.position piece) == y
    failWith error $ taking (x, y) $ S.toList $ Chess.movesFor (rankIs oy) board

explicitTake :: (Chess.Piece -> Bool) -> Chess.Board -> ChessError -> Parser Chess.Move
explicitTake p board error = do
    ox <- file
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    let pieceAt pos piece = p piece && Chess.position piece == pos
    failWith error $ taking (x, y) $ S.toList $ Chess.movesFor (pieceAt (ox, oy)) board

takePiece :: (Chess.Piece -> Bool) -> Chess.Board -> ChessError -> Parser Chess.Move
takePiece p board error = M.choice [(try $ unambigousTake p board error),
                                    (try $ fileAmbigousTake p board error),
                                    (try $ rankAmbigousTake p board error),
                                    (try $ explicitTake p board error)]

unambigousBlock :: (Chess.Piece -> Bool) -> Chess.Board -> ChessError -> Parser Chess.Move
unambigousBlock p board error = do
    x <- file
    y <- rank
    failWith error $ blocking (x, y) $ S.toList $ Chess.movesFor p board

fileAmbigousBlock :: (Chess.Piece -> Bool) -> Chess.Board -> ChessError -> Parser Chess.Move
fileAmbigousBlock p board error = do
    ox <- file
    x  <- file
    y  <- rank
    let fileIs x piece = p piece && (fst $ Chess.position piece) == x
    failWith error $ blocking (x, y) $ S.toList $ Chess.movesFor (fileIs ox) board

rankAmbigousBlock :: (Chess.Piece -> Bool) -> Chess.Board -> ChessError -> Parser Chess.Move
rankAmbigousBlock p board error = do
    oy <- rank
    x  <- file
    y  <- rank
    let rankIs y piece = p piece && (snd $ Chess.position piece) == y
    failWith error $ blocking (x, y) $ S.toList $ Chess.movesFor (rankIs oy) board

explicitBlock :: (Chess.Piece -> Bool) -> Chess.Board -> ChessError -> Parser Chess.Move
explicitBlock p board error = do
    ox <- file
    oy <- rank
    x  <- file
    y  <- rank
    let pieceAt pos piece = p piece && Chess.position piece == pos
    failWith error $ blocking (x, y) $ S.toList $ Chess.movesFor (pieceAt (ox, oy)) board

blockPiece :: (Chess.Piece -> Bool) -> Chess.Board -> ChessError -> Parser Chess.Move
blockPiece p board error = M.choice [(try $ unambigousBlock p board error),
                                     (try $ fileAmbigousBlock p board error),
                                     (try $ rankAmbigousBlock p board error),
                                     (try $ explicitBlock p board error)] 

takeOrBlock :: ChessPiece -> Chess.Board -> (Chess.Piece -> Bool) -> Parser Chess.Move
takeOrBlock piece board isPiece = takePiece isPiece board (TakeError piece) <|> blockPiece isPiece board (BlockError piece)

takePawn :: Chess.Board -> Parser Chess.Move
takePawn board = do
            ox <- file
            _  <- char 'x'
            x  <- file
            y  <- rank
            let colour = Chess.player board
                pawnAt x' (Chess.Pawn c (x'', _)) = (x' == x'') && (c == colour)
                pawnAt x' _                       = False
            failWith (TakeError Pawn) $ taking (x, y) $ S.toList $ Chess.movesFor (pawnAt ox) board

blockPawn :: Chess.Board -> Parser Chess.Move
blockPawn board = do
            x <- file
            y <- rank
            let colour = Chess.player board
                isPawn (Chess.Pawn c _) = c == colour
                isPawn _                = False
            failWith (BlockError Pawn) $ blocking (x, y) $ S.toList $ Chess.movesFor isPawn board

promotePawn :: Chess.Board -> Parser Chess.Move
promotePawn board = do
            x <- file
            y <- rank
            _ <- char '='
            p <- promotedPiece (x, y) board
            let colour = Chess.player board
                pawnAt epos (Chess.Pawn c pos) = (pos == epos) && c == colour
                pawnAt _ _                     = False
            failWith (PromoteError Pawn) $ promoting p $ S.toList $ Chess.movesFor (pawnAt (x, y)) board 

pawn :: Chess.Board -> Parser Chess.Move
pawn board = (try $ takePawn board) <|> (try $ promotePawn board) <|> (try $ blockPawn board) 

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

kingCastle :: Chess.Board -> Parser Chess.Move
kingCastle board = string "O-O" $> (Chess.kingSideCastle colour)
    where colour = Chess.player board

queenCastle :: Chess.Board -> Parser Chess.Move
queenCastle board = string "O-O-O" $> (Chess.queenSideCastle colour)
    where colour = Chess.player board

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

end :: Parser ()
end = void $ (try $ string "1-0") <|> (try $ string "1/2-1/2") <|> (try $ string "0-1")

noMove :: Chess.Board -> Parser (Chess.Board, Turn)
noMove board = end $> (board, End)

oneMove :: Chess.Board -> Parser (Chess.Board, Turn)
oneMove board = do
    _ <- lineDelimitation
    _ <- index
    _ <- delimitation
    m <- move board
    b <- applied m board
    _ <- check
    _ <- mate
    _ <- lineDelimitation
    _ <- end
    return (b, One m)

twoMove :: Chess.Board -> Parser (Chess.Board, Turn)
twoMove board = do
    _  <- lineDelimitation
    _  <- index
    _  <- lineDelimitation
    m  <- move board
    b  <- applied m board
    _  <- check
    _  <- mate
    _  <- lineDelimitation
    m' <- move b
    b' <- applied m' b
    _  <- check
    _  <- mate
    _  <- lineDelimitation
    return (b', Two (m, m'))

turn :: Chess.Board -> Parser (Chess.Board, Turn)
turn board = (try $ noMove board) <|> (try $ oneMove board) <|> (try $ twoMove board)

moveParser :: Parser [Chess.Move]
moveParser = turns [] Chess.board
        where turns moves board = turn board >>= (continue moves) 
              continue moves (board, Two (m, m')) = turns (m' : m : moves) board
              continue moves (board, One m) = continue (m : moves) (board, End)
              continue moves (board, End) = return $ reverse moves 

gameParser :: Parser Game
gameParser = do
        _      <- lineDelimitation 
        meta   <- headerParser
        _      <- lineDelimitation
        gmoves <- moveParser
        return meta { moves = gmoves }

localStringGame :: Int -> String
localStringGame n = (unsafePerformIO $ fromPGNFileString "./chess_games/batch1.pgn") !! n

fromPGNFileString :: String -> IO [String]
fromPGNFileString = fmap extract . B.readFile
    where extract = acc . (C.split '\n')
          acc []    = []
          acc lines = let (game, remaining) = extractGame lines
                      in game : (acc remaining)

fromPGNFile :: String -> IO (Either ParseError [Game])
fromPGNFile = fmap (sequence . fmap (run gameParser)) . fromPGNFileString

parseGame :: String -> Either ParseError [Chess.Move]
parseGame = fmap moves . run gameParser

parseGameIgnore :: String -> [Chess.Move]
parseGameIgnore = either (const []) moves . run gameParser

computeGameIgnore :: String -> Either Chess.Outcome Chess.Board
computeGameIgnore = foldl (\b m -> b >>= (Chess.move m)) (Right Chess.board) . parseGameIgnore

run :: (M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> Either (M.ParseErrorBundle s e) a
run p = runParser p ""

printResult :: (M.Stream s, M.ShowErrorComponent e, Show a) => Either (M.ParseErrorBundle s e) a -> IO ()
printResult (Left bundle)  = putStrLn $ M.errorBundlePretty bundle
printResult (Right result) = putStrLn $ show result

runPrint :: (Show a, M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> IO ()
runPrint p = printResult . run p