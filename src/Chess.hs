module Chess where 

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find, intersperse, unfoldr)
import Data.Tuple (swap)
import Control.Monad (foldM, mfilter)
import Control.Applicative ((<|>))
import Data.Maybe (maybe, catMaybes, fromMaybe, isNothing)

type Pos = (Int, Int)

data Colour = B | W | T deriving (Eq, Show, Ord)

data Piece = 
    Pawn Colour Pos   |
    Rook Colour Pos   |
    Bishop Colour Pos |
    Knight Colour Pos |
    Queen Colour Pos  |
    King Colour Pos   |
    Empty
    deriving (Eq, Ord)

data Move =
    Take Pos    |
    Block Pos   |
    Jump Pos    |
    KCastle     |
    QCastle
    deriving (Show, Eq, Ord)

data Board = Board {positions :: Map Pos Piece,
                    whiteKing :: Pos,
                    blackKing :: Pos}

(>?=) :: Monad f => f a -> f b -> f b
fa >?= fb = fa >>= (const fb)

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
colour (Pawn c _) = c
colour (Rook c _) = c
colour (King c _) = c
colour (Queen c _) = c
colour (Bishop c _) = c
colour (Knight c _) = c
colour Empty = T

position :: Piece -> Pos
position (Pawn _ p)   = p
position (King _ p)   = p
position (Rook _ p)   = p
position (Queen _ p)  = p
position (Bishop _ p) = p
position (Knight _ p) = p

mposition :: Move -> Pos
mposition (Take p) = p
mposition (Block p) = p
mposition (Jump p) = p

king :: Piece -> Bool
king (King _ _) = True
king _ = False

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

data Dir = U | D | L | R

steer :: Piece -> [Dir] -> Pos
steer piece = foldl shift (position piece)
    where shift (x, y) U   | white piece = (x, y + 1)
          shift (x, y) D | white piece = (x, y - 1)
          shift (x, y) U   | black piece = (x, y - 1)
          shift (x, y) D | black piece = (x, y + 1)
          shift (x, y) L  = (x - 1, y)
          shift (x, y) R = (x + 1, y)   

lookAt :: Pos -> Board -> Maybe Piece
lookAt p (Board ps _ _) = M.lookup p ps

takes :: Piece -> [Dir] -> Board -> Maybe Move
takes piece dir = fmap (const (Take pos)) . mfilter (opposite $ colour piece) . lookAt pos
    where pos = steer piece dir

blocks :: Piece -> [Dir] -> Board -> Maybe Move
blocks piece dir = fmap (const (Block pos)) . mfilter empty . lookAt pos
    where pos = steer piece dir

jumps :: Piece -> [Dir] -> Board -> Maybe Move
jumps piece dir = fmap (const (Jump pos)) . mfilter empty . lookAt pos
    where pos = steer piece dir

attacks :: Piece -> [Dir] -> Board -> Maybe Move
attacks piece dir board = lookAt pos board >>= attack
    where pos = steer piece dir
          attack piece' | opposite (colour piece) piece' = Just (Take pos)
          attack piece' | empty piece' = Just (Block pos)
          attack _ = Nothing 

pawnMoves :: Piece -> Board -> [Move]
pawnMoves pawn board = catMaybes [takes pawn  [L, U] board,
                                  takes pawn  [R, U] board,
                                  blocks pawn [U] board,
                                  jumps pawn  [U, U] board]

kingMoves :: Piece -> Board -> [Move]
kingMoves king board = catMaybes [attacks king [L, U] board,
                                  attacks king [U] board,
                                  attacks king [U, R] board,
                                  attacks king [R] board,
                                  attacks king [D, R] board,
                                  attacks king [D] board,
                                  attacks king [D, L] board,
                                  attacks king [L] board,
                                  Just QCastle,
                                  Just KCastle]

attacksR :: Piece -> [Dir] -> Board -> [Move]
attacksR piece dir board = keep $ unfoldr move piece
    where move piece = fmap (\p -> (p, p)) $ lookAt (steer piece dir) board
          keep (piece' : pieces) | empty piece = (Block $ position piece') : (keep pieces)
          keep (piece' : pieces) | (opposite (colour piece) piece') = (Take $ position piece') : (keep pieces)
          keep _ = []

rookMoves :: Piece -> Board -> [Move]
rookMoves piece board = (attacksR piece [U] board) ++ 
                        (attacksR piece [L] board) ++ 
                        (attacksR piece [R] board) ++
                        (attacksR piece [D] board)

                    
bishopMoves :: Piece -> Board -> [Move]
bishopMoves piece board = (attacksR piece [L, U] board) ++
                          (attacksR piece [R, U] board) ++
                          (attacksR piece [L, D] board) ++
                          (attacksR piece [R, D] board)

queenMoves :: Piece -> Board -> [Move]
queenMoves piece board = (rookMoves piece board) ++ (bishopMoves piece board)

knightMoves :: Piece -> Board -> [Move]
knightMoves piece board = catMaybes [attacks piece [U, U, R] board,
                                     attacks piece [U, U, L] board,
                                     attacks piece [D, D, R] board,
                                     attacks piece [D, D, L] board]

moves :: Piece -> Board -> [Move]
moves piece @ (Pawn _ _)   = pawnMoves piece
moves piece @ (Rook _ _)   = rookMoves piece
moves piece @ (Bishop _ _) = bishopMoves piece
moves piece @ (Knight _ _) = knightMoves piece 
moves piece @ (Queen _ _)  = queenMoves piece
moves piece @ (King _ _)   = kingMoves piece
moves Empty                = const []

threatsFor :: Piece -> Board -> Set Pos
threatsFor piece board = M.foldl gatherThreats S.empty $ M.filter (opposite $ colour piece) $ positions board
        where gatherThreats set piece = set <> (S.fromList $ fmap mposition $ filter threats $ moves piece board)
              threats (Take _) = True
              threats _ = False

check :: Piece -> Move -> Board -> Maybe Move 
check piece move board = if (not $ inCheck) then Just move else Nothing 
    where kingPos = if (white piece)
                    then whiteKing board 
                    else blackKing board
          inCheck = S.member kingPos $ threatsFor piece board

checkmate :: Piece -> Move -> Board -> Maybe Move
checkmate piece move board = if (not $ inCheckmate) then Just move else Nothing
    where king = if (white piece) 
                 then King W (whiteKing board)
                 else King B (blackKing board)
          inCheckmate = inCheck && cannotMove && unblockable
          inCheck     = S.member (position king) threats
          cannotMove  = S.isSubsetOf kingMoves threats
          unblockable = isNothing $ check piece move $ apply piece move board 
          kingMoves   = S.fromList $ fmap mposition $ moves king board
          threats     = threatsFor piece board

checked :: Piece -> Move -> Board -> Maybe Move
checked piece m board = check piece m $ apply piece m board 

-- Stalemate check missing
-- Chess total number of moves check missing
checks :: Piece -> Move -> Board -> Maybe Move -- maybe outcome
checks piece move board = (check piece move board) <|> (checkmate piece move board)

qcastle :: Piece -> Move -> Board -> Maybe Move
qcastle piece m board = fmap (const m) $ mfilter (const $ not blocked) $ (pure (&&) <*> hasKing <*> hasRook)
    where c = colour piece
          king p = p == (King c queenCastleKing)
          rook p = p == (Rook c queenCastleRookS)
          hasKing = fmap (const True) $ mfilter king $ lookAt queenCastleKing board
          hasRook = fmap (const True) $ mfilter rook $ lookAt queenCastleRookS board
          blocked = S.isSubsetOf safetyPath threats
          threats = threatsFor piece board
          safetyPath = S.fromList files
          files = if (white piece) 
                  then [(2, 1), (3, 1), (4, 1)]
                  else [(2, 8), (3, 8), (4, 8)]

kcastle :: Piece -> Move -> Board -> Maybe Move
kcastle piece m board = fmap (const m) $ mfilter (const (not blocked)) $ (pure (&&) <*> hasKing <*> hasRook)
    where c = colour piece
          king p = p == (King c kingCastleKing)
          rook p = p == (Rook c kingCastleRookS)
          hasKing = fmap (const True) $ mfilter king $ lookAt kingCastleKing board
          hasRook = fmap (const True) $ mfilter rook $ lookAt kingCastleRookS board
          blocked = S.isSubsetOf safetyPath threats
          threats = threatsFor piece board
          safetyPath = S.fromList files
          files = if (white piece) 
                  then [(5, 1), (6, 1), (7, 1)]
                  else [(5, 8), (6, 8), (7, 8)]   

legality :: Piece -> Move -> Board -> Maybe Move
legality p m @ (Take  _) board  = (checks p m board) >?= (checked p m board)
legality p m @ (Block _) board  = (checks p m board) >?= (checked p m board)
legality p m @ (Jump  _) board  = (checks p m board) >?= (checked p m board)
legality p m @ QCastle board    = (checks p m board) >?= (checked p m board) >?= (qcastle p m board)
legality p m @ KCastle board    = (checks p m board) >?= (checked p m board) >?= (kcastle p m board)

streamLine :: [(Pos, Pos)] -> Board -> Board
streamLine moves board = foldl transform board moves
          where transform board move = maybe board id $ adapt board move
                adapt (Board ps wk bk) (pos, pos') = do
                    piece  <- M.lookup pos ps
                    let positions = M.insert pos' piece $ M.insert pos Empty ps
                    let whiteKing = if (white piece && king piece) then pos' else wk
                    let blackKing = if (black piece && king piece) then pos' else bk
                    return Board { positions = positions, whiteKing = whiteKing, blackKing = blackKing }

apply :: Piece -> Move -> Board -> Board
apply piece KCastle = streamLine [(position piece, kingCastleKing),  (kingCastleRookS, kingCastleRookE)]
apply piece QCastle = streamLine [(position piece, queenCastleKing), (queenCastleRookS, queenCastleRookE)]
apply piece move    = streamLine [(position piece, mposition move)]

move :: Pos -> Move -> Board -> Board
move pos m board = fromMaybe board newBoard
    where newBoard = do
            piece <- lookAt pos board 
            _     <- find (== m) $ moves piece board
            _     <- legality piece m board
            return (apply piece m board)

board :: Board 
board = Board { positions = M.fromList positions, 
                whiteKing = (5, 1), 
                blackKing = (5, 8) }
    where row c y = zipWith (\x f -> ((x, y), f c (x, y))) [1..8]
          empties = [((x, y), Empty) | x <- [1..8], y <- [3..6]]
          pawns   = [Pawn, Pawn,   Pawn,   Pawn,  Pawn, Pawn,   Pawn,   Pawn]
          pieces  = [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
          positions  = (row B 8 pieces) ++
                       (row B 7 pawns) ++  
                        empties ++ 
                       (row W 2 pawns) ++ 
                       (row W 1 pieces)

instance Show Board where
    show board = unlines $ fmap row [1..8]
        where row y = foldl (++) "" $ intersperse "," $ catMaybes $ fmap (\x -> fmap show $ lookAt (x, y) board) [1..8]

instance Show Piece where
    show (Pawn W _)   = " ♙ "
    show (Pawn B _)   = " ♟ "
    show (Rook W _)   = " ♖ "
    show (Rook B _)   = " ♜ "
    show (Bishop W _) = " ♗ "
    show (Bishop B _) = " ♝ "
    show (Knight W _) = " ♘ "
    show (Knight B _) = " ♞ "
    show (Queen W _)  = " ♕ "
    show (Queen B _)  = " ♛ "
    show (King W _)   = " ♔ "
    show (King B _)   = " ♚ "
    show Empty        = "   "

--- Improvements: 
--- ✓ a) Remove Attack
--- ✓ b) Remove `pieces` field from board, replace with explicit king positions
--- ✓ c) Replace directional functions with ADT
--- d) Replace QCastle and KCastle with a single instance `Castle kingpos rookpos`
--- ✓ e) Add position to pieces
--- f) Replace list of moves with set of moves
--- g) Keep latest 3 moves on the board and disallow moves accordingly
--- h) Keep track of whose move it is and disallow moves accordingly
--- i) Keep track of the number of moves made so far
    
---- CHESS PIECES HAVE THEIR OWN UNICODE CHARACTERS!