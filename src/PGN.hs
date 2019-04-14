module PGN where

import qualified Chess as Chess
import Data.Text (Text)
import qualified Text.Megaparsec as M
import Text.Megaparsec (Parsec, (<|>), runParser)
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char (char)
import Data.Char (digitToInt)
import Control.Monad (void)
import Data.Functor (($>))
import Data.List (find)
import qualified Data.Set as S

data Result = Success | Failure deriving (Ord, Eq, Show)

type Parser a = Parsec Result String a

games :: String -> IO [String]
games filename = fmap moves $ readFile $ "../chess_games/" <> filename
    where moves = map snd . filter (even . fst) . zip [0..] . lines

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
pawn board = takePawn board <|> promotePawn board <|> blockPawn board

-- These things have a lot of implicit knowledge in them. It's annoying
-- disambiguation strategies: 
         -- 1. if the files are different, the originating file is inserted right after the piece letter
         -- 2. if the files are the same, but the ranks are different, the rank digit is inserted right after the piece letter
         -- 3. if 1. and 2. fail, the originating file and rank digit are inserted after the piece letter