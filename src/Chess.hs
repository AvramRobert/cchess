module Chess where 

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find, intersperse, unfoldr)
import Data.Tuple (swap)
import Control.Monad (foldM, mfilter)
import Control.Monad.Zip (mzip)
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
    Empty Pos
    deriving (Eq, Ord)

data Move =
    Take   (Piece, Piece)       |
    Block  (Piece, Piece)       |
    Jump   (Piece, Piece)       |
    TakeEP (Piece, Piece) Piece |
    CastleK (Piece, Piece) (Piece, Piece) |
    CastleQ (Piece, Piece) (Piece, Piece)
    deriving (Show, Eq, Ord)

data Dir = U | D | L | R

data Board = Board {positions :: Map Pos Piece,
                    pastMoves :: [Move],
                    whiteKing :: Pos,
                    blackKing :: Pos,
                    player :: Colour}

(>?=) :: Monad f => f a -> f b -> f b
fa >?= fb = fa >>= (const fb)

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
colour (Empty _) = T

position :: Piece -> Pos
position (Pawn _ p)   = p
position (King _ p)   = p
position (Rook _ p)   = p
position (Queen _ p)  = p
position (Bishop _ p) = p
position (Knight _ p) = p
position (Empty p)    = p

commit :: (Piece, Piece) -> Piece
commit ((Pawn c _), p)   = Pawn c $ position p
commit ((King c _), p)   = King c $ position p
commit ((Rook c _), p)   = Rook c $ position p
commit ((Queen c _), p)  = Queen c $ position p 
commit ((Bishop c _), p) = Bishop c $ position p
commit ((Knight c _), p) = Rook c $ position p

extract :: Move -> (Piece, Piece)
extract (Take move)      = move
extract (Block move)     = move
extract (Jump move)      = move
extract (TakeEP move _)  = move
extract (CastleK move _) = move
extract (CastleQ move _) = move

strip :: Move -> (Pos, Pos)
strip move = case (extract move) of (p, p') -> (position p, position p')

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
empty (Empty _) = True
empty _ = False

opposite :: Colour -> Piece -> Bool
opposite B = white
opposite W = black
opposite _ = (const False)

steer :: Pos -> Colour -> [Dir] -> Pos
steer pos colour = foldl (shift colour) pos
    where shift W (x, y) U = (x, y + 1)
          shift W (x, y) D = (x, y - 1)
          shift B (x, y) U = (x, y - 1)
          shift B (x, y) D = (x, y + 1)
          shift _ (x, y) L = (x - 1, y)
          shift _ (x, y) R = (x + 1, y)   

lookAt :: Pos -> Board -> Maybe Piece
lookAt p (Board ps _ _ _ _) = M.lookup p ps

enpassant :: Piece -> [Dir] -> Board -> Maybe Move
enpassant piece dir board = fmap take $ mfilter isPassant $ mzip (lookAt pos board) (lastPiece $ pastMoves board)
        where lastPiece (Jump change : _) = Just $ commit change
              lastPiece _  = Nothing
              isPassant (piece, (Pawn c p)) = (c /= clr) && (p == passPos) && (empty piece)
              take (piece', enpiece) = TakeEP (piece, piece') enpiece
              clr = colour piece
              pos = steer (position piece) clr dir 
              passPos = steer pos clr [U]

takes :: Piece -> [Dir] -> Board -> Maybe Move
takes piece dir = fmap take . mfilter (opposite clr) . lookAt pos'
    where pos' = steer (position piece) clr dir
          take piece' = Take (piece, piece')
          clr = colour piece

blocks :: Piece -> [Dir] -> Board -> Maybe Move
blocks piece dir = fmap block . mfilter empty . lookAt pos'
    where pos'  = steer (position piece) (colour piece) dir
          block piece' = Block (piece, piece')

jumps :: Piece -> [Dir] -> Board -> Maybe Move
jumps piece dir = fmap jump . mfilter empty . mfilter (const atStart) . lookAt pos'
    where pos' = steer (position piece) (colour piece) dir
          jump piece' = Jump (piece, piece')
          atStart = case (position piece) of 
                         (x, 2) -> white piece
                         (x, 7) -> black piece
                         (_, _) -> False

attacks :: Piece -> [Dir] -> Board -> Maybe Move
attacks piece dir board = lookAt pos' board >>= attack
    where pos' = steer (position piece) (colour piece) dir
          attack piece' | opposite (colour piece) piece' = Just $ Take (piece, piece')
          attack piece' | empty piece' = Just $ Block (piece, piece')
          attack _ = Nothing

attacksR :: Piece -> [Dir] -> Board -> [Move]
attacksR piece dir board = keep $ unfoldr move piece
    where move p = fmap (\piece' -> ((piece, piece'), piece')) $ lookAt (steer (position p) clr dir) board
          keep (p : pieces) | (empty $ snd p) = (Block p) : (keep pieces)
          keep (p : pieces) | (opponent $ snd p) = (Take p) : (keep pieces)
          keep _ = []
          clr  = colour piece
          opponent = opposite (colour piece)

-- Once the king or rooks move, you cannot castle anymore
castlesK :: Piece -> Board -> Maybe Move
castlesK piece = fmap (const castle) . mfilter inPlace . lookAt rookPos
    where inPlace rook = (rook == eRook) && (piece == eKing)
          rookPos  = if (white piece) then (8, 1) else (8, 8)
          rookPos' = if (white piece) then (6, 1) else (6, 8)
          kingPos  = if (white piece) then (5, 1) else (5, 8)
          kingPos' = if (white piece) then (7, 1) else (7, 8)
          eRook = Rook (colour piece) rookPos
          eKing = King (colour piece) kingPos
          castle = CastleK (eKing, Empty kingPos') (eRook, Empty rookPos')
    
castlesQ :: Piece -> Board -> Maybe Move
castlesQ piece = fmap (const castle) . mfilter inPlace . lookAt rookPos
    where inPlace rook = (rook == eRook) && (piece == eKing)
          rookPos  = if (white piece) then (1, 1) else (1, 8)
          rookPos' = if (white piece) then (4, 1) else (4, 8)
          kingPos  = if (white piece) then (5, 1) else (5, 8)
          kingPos' = if (white piece) then (3, 1) else (3, 8)
          eRook = Rook (colour piece) rookPos
          eKing = King (colour piece) kingPos
          castle = CastleQ (eKing, Empty kingPos') (eRook, Empty rookPos')

streamLine :: [(Piece, Piece)] -> Board -> Board
streamLine changes board = foldl transform board changes
          where transform board change = board { positions = perform board change }
                perform board change @ (piece, piece') = M.insert (position piece') (commit change) $ 
                                                         M.insert (position piece) (Empty (position piece)) $ 
                                                         positions board

pawnMoves :: Piece -> Board -> [Move]
pawnMoves pawn board = catMaybes [enpassant pawn [R, D] board,
                                  enpassant pawn [L, D] board,
                                  takes pawn  [L, U] board,
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
                                  castlesK king board,
                                  castlesQ king board]

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
moves piece @ (Empty _)    = const []

-- Thing is, if I take the moves, I have to commit them in order to see which squares are affacted, don't I?
threatsFor :: Piece -> Board -> Set Pos
threatsFor piece board = M.foldl gatherThreats S.empty $ M.filter (opposite $ colour piece) $ positions board
        where gatherThreats set piece = set <> (S.fromList $ fmap (position . commit . extract) $ filter threats $ moves piece board)
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
          unblockable = isNothing $ check piece move $ apply move board 
          kingMoves   = S.fromList $ fmap (position . commit . extract) $ moves king board
          threats     = threatsFor piece board

checked :: Piece -> Move -> Board -> Maybe Move
checked piece move = check piece move . apply move 

-- Stalemate check missing
-- Chess total number of moves check missing
checks :: Piece -> Move -> Board -> Maybe Move -- maybe outcome
checks piece move board = (check piece move board) <|> (checkmate piece move board)

qcastle :: Piece -> Move -> Board -> Maybe Move
qcastle piece m board = if free then Just m else Nothing
    where free = S.null $ S.intersection safetyPath $ threatsFor piece board
          safetyPath = S.fromList files
          files = if (white piece) 
                  then [(2, 1), (3, 1), (4, 1)]
                  else [(2, 8), (3, 8), (4, 8)]

kcastle :: Piece -> Move -> Board -> Maybe Move
kcastle piece m board = if free then Just m else Nothing
    where free = S.null $ S.intersection safetyPath $ threatsFor piece board
          safetyPath = S.fromList files
          files = if (white piece) 
                  then [(5, 1), (6, 1), (7, 1)]
                  else [(5, 8), (6, 8), (7, 8)]   

changeTurn :: Board -> Board
changeTurn board = board { player = invert $ player board }

adjustKings :: (Piece, Piece) -> Board -> Board
adjustKings (p, _) board | white p && king p = board { whiteKing = position p }
adjustKings (p, _) board | black p && king p = board { blackKing = position p }
adjustKings _ board = board

accumulate :: Move -> Board -> Board
accumulate move board = board { pastMoves = move : (pastMoves board) }

apply :: Move -> Board -> Board
apply move @ (CastleK kingMove rookMove) = changeTurn . accumulate move . adjustKings kingMove . streamLine [kingMove, rookMove]
apply move @ (CastleQ kingMove rookMove) = changeTurn . accumulate move . adjustKings kingMove . streamLine [kingMove, rookMove]
apply move @ (TakeEP pawnMove piece) = changeTurn . accumulate move . streamLine [pawnMove, (piece, piece)]
apply move @ (Jump  pieceMove)   = changeTurn . accumulate move . streamLine [pieceMove]
apply move @ (Take  pieceMove)   = changeTurn . accumulate move . adjustKings pieceMove . streamLine [pieceMove]
apply move @ (Block pieceMove)   = changeTurn . accumulate move . adjustKings pieceMove . streamLine [pieceMove]

legality :: Piece -> Move -> Board -> Maybe Move
legality p m @ (Take  _) board    = (checks p m board) >?= (checked p m board)
legality p m @ (Block _) board    = (checks p m board) >?= (checked p m board)
legality p m @ (Jump  _) board    = (checks p m board) >?= (checked p m board)
legality p m @ (CastleK _ _) board = (checks p m board) >?= (checked p m board) >?= (kcastle p m board)
legality p m @ (CastleQ _ _) board = (checks p m board) >?= (checked p m board) >?= (qcastle p m board)

move :: Pos -> Pos -> Board -> Board
move pos pos' board = fromMaybe board newBoard
    where newBoard = do
            piece <- lookAt pos board 
            move  <- find (available . strip) $ moves piece board
            _     <- legality piece move board
            return (apply move board)
          available  move = move == (pos, pos')
    
board :: Board 
board = Board { positions = M.fromList positions,
                pastMoves = [],
                whiteKing = (5, 1), 
                blackKing = (5, 8),
                player    = W }
    where row c y = zipWith (\x f -> ((x, y), f c (x, y))) [1..8]
          empties = [((x, y), (Empty (x, y))) | x <- [1..8], y <- [3..6]]
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
    show (Empty _)    = "   "

--- Improvements: 
--- ✓ a) Remove Attack
--- ✓ b) Remove `pieces` field from board, replace with explicit king positions
--- ✓ c) Replace directional functions with ADT
--- ✓ d) Replace QCastle and KCastle with a single instance `Castle kingpos rookpos`
--- ✓ e) Add position to pieces
--- f) Replace list of moves with set of moves
--- ✓ g) Keep all moves on the board and disallow moves accordingly
--- h) Keep track of whose move it is and disallow moves accordingly
    
---- CHESS PIECES HAVE THEIR OWN UNICODE CHARACTERS!