module Chess where 

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find, intersperse)
import Data.Tuple (swap)
import Control.Monad (foldM, mfilter)
import Control.Applicative ((<|>))
import Data.Maybe (maybe, catMaybes, fromMaybe, isNothing)

type Pos = (Int, Int)

data Colour = B | W | T deriving (Eq, Show, Ord)

data Piece = 
    Pawn Int Colour   |
    Rook Int Colour   |
    Bishop Int Colour |
    Knight Int Colour |
    Queen Colour      |
    King Colour       |
    Empty
    deriving (Eq, Ord)

data Move =
    Take Pos    |
    Block Pos   |
    Attack Pos  |
    Jump Pos    |
    KCastle     |
    QCastle
    deriving (Show, Eq, Ord)

data Board = Board {positions :: Map Pos Piece, pieces :: Map Piece Pos}

(>?=) :: Monad f => f a -> f b -> f b
fa >?= fb = fa >>= (const fb)

filterByKey :: (k -> Bool) -> Map k v -> Map k v
filterByKey p = M.filterWithKey (\k _ -> p k)

kingCastleRookS  = (8, 1)
queenCastleRookS = (1, 1)
kingCastleRookE  = (6, 1)
queenCastleRookE = (4, 1)
kingCastleKing   = (7, 1)
queenCastleKing  = (3, 1)

invert :: Colour -> Colour
invert B = W
invert W = B
invert T = T

colour :: Piece -> Colour
colour (Pawn _ c) = c
colour (Rook _ c) = c
colour (King c) = c
colour (Queen c) = c
colour (Bishop _ c) = c
colour (Knight _ c) = c
colour Empty = T

left :: Colour -> Pos -> Pos
left _ (x, y) = (x - 1, y)

right :: Colour -> Pos -> Pos
right _ (x, y) = (x + 1, y)

up :: Colour -> Pos -> Pos
up W (x, y) = (x, y + 1)
up B (x, y) = (x, y - 1)

down :: Colour -> Pos -> Pos
down W (x, y) = (x, y - 1)
down B (x, y) = (x, y + 1)

leftUp :: Colour -> Pos -> Pos
leftUp c = (left c) . (up c)

leftDown :: Colour -> Pos -> Pos
leftDown c = (left c) . (down c)

rightUp :: Colour -> Pos -> Pos
rightUp c = (right c) . (up c)

rightDown :: Colour -> Pos -> Pos
rightDown c = (right c) . (down c)

black :: Piece -> Bool
black = (== B) . colour

white :: Piece -> Bool
white = (== W) . colour

empty :: Piece -> Bool
empty Empty = True
empty _ = False

opposite :: Colour -> Piece -> Bool
opposite B = white
opposite W = black
opposite _ = (const False)

lookAt:: Pos -> Board -> Maybe Piece
lookAt p (Board ps _) = M.lookup p ps

lookFor :: Piece -> Board -> Maybe Pos
lookFor pc (Board _ pcs) = M.lookup pc pcs

attack :: Pos -> Colour -> Board -> Maybe Move
attack pos colour = fmap (const (Attack pos)) . mfilter viable . lookAt pos
        where viable piece = (opposite colour piece) || (empty piece)

take' :: Pos -> Colour -> Board -> Maybe Move
take' pos colour = fmap (const (Take pos)) . mfilter (opposite colour) . lookAt pos

block :: Pos -> Board -> Maybe Move
block pos = fmap (const (Block pos)) . mfilter empty . lookAt pos

jump :: Pos -> Board -> Maybe Move
jump pos = fmap (const (Jump pos)) . mfilter empty . lookAt pos

-- DO BETTER. MOVE FROM LIST TO SET
attackf :: (Colour -> Pos -> Pos) -> Pos -> Colour -> Board -> [Move]
attackf f pos colour board = keepValid $ iterate (f colour) pos -- Do better, create a set directly 
    where keepValid (pos : ps) = let piece = lookAt pos board
                                 in keep piece ps 
          keep (Just piece) ps | empty piece = (Attack pos) : (keepValid ps) 
          keep (Just piece) ps | opposite colour piece = (Attack pos) : []
          keep _ _ = []

-- En Passant move missing
-- Jumps shouldn't be allowed after jumping once
pawnMoves :: Pos -> Colour -> Board -> [Move]
pawnMoves p colour board = catMaybes [take' (leftUp colour p)  colour board, 
                                      take' (rightUp colour p) colour board,
                                      block (up colour p) board,
                                      jump  (up colour (up colour p)) board]

kingMoves :: Pos -> Colour -> Board -> [Move]
kingMoves p colour board = catMaybes [attack (leftUp colour p) colour board, 
                                      attack (up colour p) colour board, 
                                      attack (rightUp colour p) colour board, 
                                      attack (right colour p) colour board,
                                      attack (rightDown colour p) colour board,
                                      attack (down colour p) colour board, 
                                      attack (leftDown colour p) colour board, 
                                      attack (left colour p) colour board,
                                      Just QCastle,
                                      Just KCastle]

rookMoves :: Pos -> Colour -> Board -> [Move]
rookMoves p colour board = (attackf up p colour board)    ++ 
                           (attackf left p colour board)  ++
                           (attackf right p colour board) ++
                           (attackf down p colour board)

bishopMoves :: Pos -> Colour -> Board -> [Move]
bishopMoves p colour board = (attackf leftUp p colour board)    ++
                             (attackf rightUp p colour board)   ++
                             (attackf leftDown p colour board)  ++
                             (attackf rightDown p colour board)

queenMoves :: Pos -> Colour -> Board -> [Move]
queenMoves p colour board = (rookMoves p colour board) ++ (bishopMoves p colour board)

knightMoves :: Pos -> Colour -> Board -> [Move]
knightMoves p colour board = catMaybes [attack (right colour (up colour (up colour p))) colour board,
                                        attack (right colour (down colour (down colour p))) colour board,
                                        attack (left colour (down colour (down colour p))) colour board,
                                        attack (left colour (up colour (up colour p))) colour board]

moves :: Pos -> Piece -> Board -> [Move]
moves pos (Pawn _ colour)   = pawnMoves pos colour
moves pos (Rook _ colour)   = rookMoves pos colour
moves pos (Bishop _ colour) = bishopMoves pos colour
moves pos (Knight _ colour) = knightMoves pos colour 
moves pos (Queen colour)    = queenMoves pos colour
moves pos (King  colour)    = kingMoves pos colour
moves pos Empty             = const []

mposition :: Move -> Pos
mposition (Take p) = p
mposition (Block p) = p
mposition (Attack p) = p

threatsOf :: Colour -> Board -> Set Pos
threatsOf colour board = M.foldlWithKey gatherThreats S.empty $ filterByKey (opposite colour) $ pieces board
        where gatherThreats set piece pos = set <> (S.fromList $ fmap mposition $ filter threats $ moves pos piece board)
              threats (Attack _) = True
              threats (Take _) = True
              threats _ = False

check :: Pos -> Piece -> Move -> Board -> Maybe Move 
check _ piece m board = fmap (const m) $ mfilter (not . inCheck) $ M.lookup king $ pieces board
    where ocolour = invert $ colour piece
          king = King ocolour
          inCheck pos = S.member pos threats
          threats = threatsOf ocolour board

checkmate :: Pos -> Piece -> Move -> Board -> Maybe Move
checkmate pos piece m board = fmap (const m) $ mfilter (not . inCheckmate) $ M.lookup king $ pieces board
    where ocolour = invert $ colour piece
          king = King ocolour
          inCheckmate pos = (inCheck pos) && (cannotMove pos) && unblockable
          inCheck kingPos = S.member kingPos threats
          cannotMove kingPos = S.isSubsetOf (kingMoves kingPos) threats
          unblockable = isNothing $ check pos piece m $ apply pos m board 
          kingMoves kingPos = S.fromList $ fmap mposition $ moves kingPos king board
          threats = threatsOf ocolour board

checked :: Pos -> Piece -> Move -> Board -> Maybe Move
checked pos piece m board = check pos piece m $ apply pos m board 

-- Stalemate check missing
-- Chess total number of moves check missing
checks :: Pos -> Piece -> Move -> Board -> Maybe Move -- maybe outcome
checks pos piece move board = (check pos piece move board) <|> (checkmate pos piece move board)

qcastle :: Pos -> Piece -> Move -> Board -> Maybe Move
qcastle _ piece m board = fmap (const m) $ mfilter (const (not blocked)) $ (pure (&&) <*> hasKing <*> hasRook)
    where c = colour piece
          king p = p == (King c)
          rook p = (p == (Rook 1 c)) || (p == (Rook 2 c))
          hasKing = fmap (const True) $ mfilter king $ lookAt queenCastleKing board
          hasRook = fmap (const True) $ mfilter rook $ lookAt queenCastleRookS board
          blocked = S.isSubsetOf safetyPath threats
          safetyPath = S.fromList [(7, 1), (6, 1), (5, 1)]
          threats = threatsOf (invert c) board        

kcastle :: Pos -> Piece -> Move -> Board -> Maybe Move
kcastle _ piece m board = fmap (const m) $ mfilter (const (not blocked)) $ (pure (&&) <*> hasKing <*> hasRook)
            where c = colour piece
                  king p = p == (King c)
                  rook p = (p == (Rook 1 c)) || (p == (Rook 2 c))
                  hasKing = fmap (const True) $ mfilter king $ lookAt kingCastleKing board
                  hasRook = fmap (const True) $ mfilter rook $ lookAt kingCastleRookS board
                  blocked = S.isSubsetOf safetyPath threats
                  safetyPath = S.fromList [(7, 1), (6, 1), (5, 1)]
                  threats = threatsOf (invert c) board        

legality :: Pos -> Piece -> Move -> Board -> Maybe Move
legality pos p m @ (Take pos') board   = (checks pos p m board) >?= (checked pos p m board)
legality pos p m @ (Block pos') board  = (checks pos p m board) >?= (checked pos p m board)
legality pos p m @ (Attack pos') board = (checks pos p m board) >?= (checked pos p m board)
legality pos p m @ (Jump pos') board   = (checks pos p m board) >?= (checked pos p m board)
legality pos p m @ QCastle board       = (checks pos p m board) >?= (checked pos p m board) >?= (qcastle pos p m board)
legality pos p m @ KCastle board       = (checks pos p m board) >?= (checked pos p m board) >?= (kcastle pos p m board)

streamLine :: [(Pos, Pos)] -> Board -> Board
streamLine moves board = foldl transform board moves
    where transform board change = maybe board id (adapt board change)
          adapt (Board ps pcs) (pos, pos') = do
            piece  <- M.lookup pos  ps
            piece' <- M.lookup pos' ps
            let positions = M.insert pos' piece $ M.insert pos Empty ps
            let pieces = M.delete piece' $ M.insert piece pos' pcs
            return $ Board { positions = positions, pieces = pieces }

apply :: Pos -> Move -> Board -> Board
apply pos (Take pos')   = streamLine [(pos, pos')]
apply pos (Block pos')  = streamLine [(pos, pos')]
apply pos (Attack pos') = streamLine [(pos, pos')]
apply pos (Jump pos')   = streamLine [(pos, pos')]
apply pos KCastle       = streamLine [(pos, kingCastleKing),  (kingCastleRookS, kingCastleRookE)]
apply pos QCastle       = streamLine [(pos, queenCastleKing), (queenCastleRookS, queenCastleRookE)]

move :: Pos -> Move -> Board -> Board
move pos m board = fromMaybe board newBoard
    where newBoard = do
            piece <- lookAt pos board 
            _     <- find (== m) $ moves pos piece board
            _     <- legality pos piece m board
            return (apply pos m board)

board :: Board 
board = Board { positions = M.fromList ps, pieces = M.fromList pcs }
    where pawns c y = fmap (\x -> ((x, y), Pawn x c)) [1..8]
          pieces c y = zipWith (\x p -> ((x, y), p)) [1..8] (row c)
          row c = [Rook 1 c, Knight 1 c, Bishop 1 c, Queen c, King c, Bishop 2 c, Knight 2 c, Rook 2 c]
          empties = [((x, y), Empty) | x <- [1..8], y <- [3..6]]
          ps  = (pawns B 7) ++ (pieces B 8) ++ (pawns W 2) ++ (pieces W 1) ++ empties
          pcs = fmap swap ps


instance Show Board where
    show board = unlines $ fmap row [1..8]
        where row y = foldl (++) "" $ intersperse "," $ catMaybes $ fmap (\x -> fmap show $ lookAt (x, y) board) [1..8]

instance Show Piece where
    show (Pawn _ W)   = " ♙ "
    show (Pawn _ B)   = " ♟ "
    show (Rook _ W)   = " ♖ "
    show (Rook _ B)   = " ♜ "
    show (Bishop _ W) = " ♗ "
    show (Bishop _ B) = " ♝ "
    show (Knight _ W) = " ♘ "
    show (Knight _ B) = " ♞ "
    show (Queen W)    = " ♕ "
    show (Queen B)    = " ♛ "
    show (King W)     = " ♔ "
    show (King B)     = " ♚ "
    show Empty        = "   "

--- Improvements: 
--- a) Remove Attack
--- b) Remove `pieces` field from board, replace with explicit king positions
--- c) Replace directional functions with ADT
--- d) Replace QCastle and KCastle with a single instance `Castle kingpos rookpos`
--- e) Add position to pieces
--- f) Replace list of moves with set of moves
--- g) Keep latest 3 moves on the board and disallow moves accordingly
--- h) Keep track of whose move it is and disallow moves accordingly
--- i) Keep track of the number of moves made so far
    
---- CHESS PIECES HAVE THEIR OWN UNICODE CHARACTERS!