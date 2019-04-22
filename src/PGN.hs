{-# LANGUAGE DeriveAnyClass #-}

module PGN where

import qualified Chess as Chess
import Data.Text (Text)
import qualified Text.Megaparsec as M
import Text.Megaparsec (Parsec, (<|>), runParser, try)
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char (char, string, spaceChar)
import Data.Char (digitToInt)
import Control.Monad (void)
import Data.Functor (($>))
import Data.List (find)
import qualified Data.Set as S

data Result = Success | Failure deriving (Ord, Eq, Show, M.ShowErrorComponent)

type Parser a = Parsec Result String a

stripNewLines :: String -> String
stripNewLines = filter (not . newLine) 
    where newLine '\n' = True
          newLine '\r' = True
          newLine  _   = False

extractGame :: [String] -> (String, [String])
extractGame pgnLines = (game, remLines)
    where game = stripNewLines $ unlines gameLines
          gameLines = takeWhile (not . newLine) headless
          headless  = drop 12 pgnLines
          remLines  = drop ((length gameLines) + 1) headless
          newLine "\n" = True
          newLine "\r" = True
          newLine _    = False

games :: String -> IO [String]
games filename = fmap parseGames $ readFile $ "./chess_games/" <> filename
    where parseGames = acc [] . lines
          acc gms [] = reverse gms
          acc gms lns = let (game, lns') = extractGame lns 
                        in acc (game : gms) lns'

index :: Parser Int
index = do
    i <- C.digitChar
    _ <- C.char '.'
    return $ digitToInt i

file :: Parser Int
file = (char 'a' $> 1) <|>
       (char 'b' $> 2) <|> 
       (char 'c' $> 3) <|>
       (char 'd' $> 4) <|>
       (char 'e' $> 5) <|>
       (char 'f' $> 6) <|>
       (char 'g' $> 7) <|>
       (char 'h' $> 8) 

piece :: Chess.Pos -> Chess.Board -> Parser Chess.Piece
piece pos board = (char 'Q' >> (convert Chess.Queen))  <|> 
                  (char 'R' >> (convert Chess.Rook))   <|>
                  (char 'N' >> (convert Chess.Knight)) <|>
                  (char 'B' >> (convert Chess.Bishop))
    where 
        promote f piece = f (Chess.colour piece) (Chess.position piece) 
        convert f = parsedReturn $ fmap (promote f) $ Chess.lookAt pos board

rank :: Parser Int
rank = fmap digitToInt $ C.numberChar

check :: Parser ()
check = void $ M.optional $ char '+'

mate :: Parser ()
mate = void $ M.optional $ char '#'

taking :: Chess.Pos -> [Chess.Move] -> Maybe Chess.Move
taking square = find take
    where take (Chess.Take piece piece')     = (Chess.colour piece /= Chess.colour piece') && (Chess.position piece' == square)
          take (Chess.TakeEP piece piece' _) = (Chess.colour piece /= Chess.colour piece') && (Chess.position piece' == square)
          take _ = False

blocking :: Chess.Pos -> [Chess.Move] -> Maybe Chess.Move
blocking square = find move
    where move (Chess.Block piece (Chess.Empty square')) = square == square'
          move (Chess.Jump piece (Chess.Empty square')) = square == square'
          move _ = False

promoting :: Chess.Piece -> [Chess.Move] -> Maybe Chess.Move
promoting piece = find promotion
    where promotion (Chess.Promote _ piece') = piece == piece' 

parsedReturn :: Maybe a -> Parser a
parsedReturn (Just a) = return a
parsedReturn (Nothing)   = M.failure Nothing S.empty

movesWhere :: (Chess.Piece -> Bool) -> Chess.Board -> [Chess.Move]
movesWhere p board = Chess.piecesWhere p board >>= (\p -> S.toList $ Chess.moves p board) -- I think this should stay a Set

unambigousTake :: (Chess.Piece -> Bool) -> Chess.Board -> Parser Chess.Move
unambigousTake p board = do 
    _ <- char 'x'
    x <- file
    y <- rank
    _ <- check <|> mate
    parsedReturn $ taking (x, y) $ movesWhere p board

fileAmbigousTake :: (Chess.Piece -> Bool) -> Chess.Board -> Parser Chess.Move
fileAmbigousTake p board = do
    ox <- file
    _  <- char 'x'
    x  <- file
    y  <- rank
    _  <- check <|> mate
    let fileAt ox piece = p piece && (fst $ Chess.position piece) == ox
    parsedReturn $ taking (x, y) $ movesWhere (fileAt ox) board 

rankAmbigousTake :: (Chess.Piece -> Bool) -> Chess.Board -> Parser Chess.Move
rankAmbigousTake p board = do
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    _  <- check <|> mate
    let rankAt oy piece = p piece && (snd $ Chess.position piece) == oy
    parsedReturn $ taking (x, y) $ movesWhere (rankAt oy) board

explicitTake :: (Chess.Piece -> Bool) -> Chess.Board -> Parser Chess.Move
explicitTake p board = do
    ox <- file
    oy <- rank
    _  <- char 'x'
    x  <- file
    y  <- rank
    let pieceAt pos piece = p piece && Chess.position piece == pos
    parsedReturn $ taking (x, y) $ movesWhere (pieceAt (ox, oy)) board

takePiece :: (Chess.Piece -> Bool) -> Chess.Board -> Parser Chess.Move
takePiece p board = (try $ unambigousTake p board) <|> (try $ fileAmbigousTake p board) <|> (try $ rankAmbigousTake p board) <|> (try $ explicitTake p board)

unambigousBlock :: (Chess.Piece -> Bool) -> Chess.Board -> Parser Chess.Move
unambigousBlock p board = do
    x <- file
    y <- rank
    _ <- check <|> mate
    parsedReturn $ blocking (x, y) $ movesWhere p board

fileAmbigousBlock :: (Chess.Piece -> Bool) -> Chess.Board -> Parser Chess.Move
fileAmbigousBlock p board = do
    ox <- file
    x  <- file
    y  <- rank
    _  <- check <|> mate
    let fileAt ox piece = p piece && (fst $ Chess.position piece) == ox
    parsedReturn $ blocking (x, y) $ movesWhere (fileAt ox) board

rankAmbigousBlock :: (Chess.Piece -> Bool) -> Chess.Board -> Parser Chess.Move
rankAmbigousBlock p board = do
    oy <- rank
    x  <- file
    y  <- rank
    _  <- check <|> mate
    let rankAt oy piece = p piece && (snd $ Chess.position piece) == oy
    parsedReturn $ blocking (x, y) $ movesWhere (rankAt oy) board

explicitBlock :: (Chess.Piece -> Bool) -> Chess.Board -> Parser Chess.Move
explicitBlock p board = do
    ox <- file
    oy <- rank
    x  <- file
    y  <- rank
    _  <- check <|> mate
    let pieceAt pos piece = p piece && Chess.position piece == pos
    parsedReturn $ blocking (x, y) $ movesWhere (pieceAt (ox, oy)) board

blockPiece :: (Chess.Piece -> Bool) -> Chess.Board -> Parser Chess.Move
blockPiece p board = (try $ unambigousBlock p board) <|> (try $ fileAmbigousBlock p board) <|> (try $ rankAmbigousTake p board) <|> (try $ explicitBlock p board)

takePawn :: Chess.Board -> Parser Chess.Move
takePawn board = do
        ox <- file
        _  <- char 'x'
        x  <- file
        y  <- rank
        _  <- check <|> mate
        let pawnAt x' (Chess.Pawn _ (x'', _)) = x' == x''
            pawnAt x' _ = False
        parsedReturn $ taking (x, y) $ movesWhere (pawnAt ox) board

blockPawn :: Chess.Board -> Parser Chess.Move
blockPawn board = do
            x <- file
            y <- rank
            _ <- check <|> mate
            let isPawn (Chess.Pawn _ _) = True
                isPawn _ = False
            parsedReturn $ blocking (x, y) $ movesWhere isPawn board

promotePawn :: Chess.Board -> Parser Chess.Move
promotePawn board = do
            x <- file
            y <- rank
            _ <- char '='
            p <- piece (x, y) board
            _ <- check <|> mate
            let pawnAt epos (Chess.Pawn _ pos) = pos == epos
                pawnAt _ _ = False
            parsedReturn $ promoting p $ movesWhere (pawnAt (x, y)) board 

pawn :: Chess.Board -> Parser Chess.Move
pawn board = (try $ takePawn board) <|> (try $ promotePawn board) <|> (try $ blockPawn board) 

rook :: Chess.Board -> Parser Chess.Move
rook board = char 'R' >> (takePiece isRook board <|> blockPiece isRook board)
    where isRook (Chess.Rook _ _) = True
          isRook _ = False

bishop :: Chess.Board -> Parser Chess.Move
bishop board = char 'B' >> (takePiece isBishop board <|> blockPiece isBishop board)
    where isBishop (Chess.Bishop _ _) = True
          isBishop _ = False

knight :: Chess.Board -> Parser Chess.Move
knight board = char 'N' >> (takePiece isKnight board <|> blockPiece isKnight board)
    where isKnight (Chess.Knight _ _) = True
          isKnight _ = False

queen :: Chess.Board -> Parser Chess.Move
queen board = char 'Q' >> (takePiece isQueen board <|> blockPiece isQueen board)
    where isQueen (Chess.Queen _ _) = True
          isQueen _ = False

king :: Chess.Board -> Parser Chess.Move
king board = char 'K' >> (takePiece isKing board <|> blockPiece isKing board)
    where isKing (Chess.King _ _) = True
          isKing _ = False

kingCastle :: Chess.Board -> Parser Chess.Move
kingCastle board = do
        _ <- string "O-O"
        _ <- check <|> mate
        let colour = Chess.player board
            white  = colour == Chess.W
            king   = if white then Chess.King colour (5, 1)
                              else Chess.King colour (5, 8)
            rook   = if white then Chess.Rook colour (8, 1)
                              else Chess.Rook colour (8, 8)
            eKing  = if white then Chess.Empty (7, 1)
                              else Chess.Empty (7, 8)
            eRook  = if white then Chess.Empty (6, 1)
                              else Chess.Empty (6, 8)
            castle = Chess.CastleK (Chess.Block king eKing) (Chess.Block rook eRook)
        parsedReturn $ Just castle

queenCastle :: Chess.Board -> Parser Chess.Move
queenCastle board = do
        _ <- string "O-O-O"
        _ <- check <|> mate
        let colour = Chess.player board
            white  = colour == Chess.W
            king   = if white then Chess.King colour (5, 1) 
                              else Chess.King colour (5, 8)
            rook   = if white then Chess.Rook colour (1, 1)
                              else Chess.Rook colour (1, 8)
            eKing  = if white then Chess.Empty (3, 1) 
                              else Chess.Empty (3, 8)
            eRook  = if white then Chess.Empty (4, 1) 
                              else Chess.Empty (4, 8)
            castle = Chess.CastleQ (Chess.Block king eKing) (Chess.Block rook eRook)
        parsedReturn $ Just castle

castle :: Chess.Board -> Parser Chess.Move
castle board = kingCastle board <|> queenCastle board

end :: Parser ()
end = void $ string "1-0" <|> string "1/2-1/2" <|> string "0-1"

move :: Chess.Board -> Parser Chess.Move
move board = (try $ pawn board)   <|>
             (try $ king board)   <|>
             (try $ rook board)   <|>
             (try $ bishop board) <|> 
             (try $ knight board) <|>
             (try $ queen  board)

data Turn = End | One Chess.Move | Two (Chess.Move, Chess.Move) deriving (Show, Eq) 

applied :: Chess.Move -> Chess.Board -> Parser Chess.Board
applied move board = case Chess.move move board of
                          (Chess.Continue, board) -> pure board
                          (_, _) -> parsedReturn Nothing

noMove :: Chess.Board -> Parser (Chess.Board, Turn)
noMove board = end $> (board, End)

oneMove :: Chess.Board -> Parser (Chess.Board, Turn)
oneMove board = do
    _ <- index
    m <- move board
    b <- applied m board
    _ <- end
    return (b, One m)

twoMove :: Chess.Board -> Parser (Chess.Board, Turn)
twoMove board = do
    _  <- index
    m  <- move board
    b  <- applied m board
    _  <- spaceChar
    m' <- move board
    b' <- applied m' b
    _  <- spaceChar
    return (b', Two (m, m'))

turn :: Chess.Board -> Parser (Chess.Board, Turn)
turn board = (try $ noMove board) <|> (try $ oneMove board) <|> (try $ twoMove board)

gameParser :: Parser [Chess.Move]
gameParser = turns [] Chess.board
        where turns moves board = turn board >>= (continue moves) 
              continue moves (board, Two (m, m')) = turns (m' : m : moves) board
              continue moves (board, One m) = turns (m : moves) board
              continue moves (board, End) = return $ reverse moves 

run :: (M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> Either (M.ParseErrorBundle s e) a
run p = runParser p ""

printErr :: (M.Stream s, M.ShowErrorComponent e) => Either (M.ParseErrorBundle s e) a -> IO ()
printErr (Left bundle) = putStrLn $ M.errorBundlePretty bundle
printErr (Right _) = putStrLn "No ERROR!"

runPrint :: (Show a, M.Stream s, M.ShowErrorComponent e) => M.Parsec e s a -> s -> IO ()
runPrint p = printErr . run p

-- These things have a lot of implicit knowledge in them. It's annoying
-- disambiguation strategies: 
         -- 1. if the files are different, the originating file is inserted right after the piece letter
         -- 2. if the files are the same, but the ranks are different, the rank digit is inserted right after the piece letter
         -- 3. if 1. and 2. fail, the originating file and rank digit are inserted after the piece letter
