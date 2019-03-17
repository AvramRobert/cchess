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
import Data.Maybe (maybe, catMaybes, fromMaybe, isNothing, isJust)
import System.CPUTime (getCPUTime)
import System.IO.Unsafe (unsafePerformIO)
import Text.Printf

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
    Take    Piece Piece       |
    Block   Piece Piece       |
    Jump    Piece Piece       |
    TakeEP  Piece Piece Piece |
    Promote Piece Piece       |
    CastleK Move Move         |
    CastleQ Move Move
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
extract (Take p p')      = (p, p')
extract (Block p p')     = (p, p')
extract (Jump p p')      = (p, p')
extract (TakeEP p p' _)  = (p, p')
extract (CastleK move _) = extract move
extract (CastleQ move _) = extract move

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
        where lastPiece ((Jump piece piece') : _) = Just $ commit (piece, piece')    
              lastPiece _  = Nothing
              isPassant (piece, (Pawn c p)) = (c /= clr) && (p == passPos) && (empty piece)
              take (piece', enpiece) = TakeEP piece piece' enpiece
              clr = colour piece
              pos = steer (position piece) clr dir 
              passPos = steer pos clr [U]

promote :: Piece -> Piece -> Board -> Maybe Move
promote piece piece' board = case piece of
                (Pawn c (_, 8)) | white piece && otherPiece -> Just (Promote piece piece')
                (Pawn c (_, 1)) | black piece && otherPiece -> Just (Promote piece piece')
                _ -> Nothing 
        where otherPiece = (not $ pawn piece') && (not $ king piece')
              pawn (Pawn _ _) = True
              pawn _ = False 

takes :: Piece -> [Dir] -> Board -> Maybe Move
takes piece dir = fmap take . mfilter (opposite clr) . lookAt pos'
    where pos'  = steer (position piece) clr dir
          take  = Take piece
          clr   = colour piece

blocks :: Piece -> [Dir] -> Board -> Maybe Move
blocks piece dir = fmap block . mfilter empty . lookAt pos'
    where pos'   = steer (position piece) (colour piece) dir
          block  = Block piece

jumps :: Piece -> [Dir] -> Board -> Maybe Move
jumps piece dir = fmap jump . mfilter empty . mfilter (const atStart) . lookAt pos'
    where pos' = steer (position piece) (colour piece) dir
          jump = Jump piece
          atStart = case (position piece) of 
                         (x, 2) -> white piece
                         (x, 7) -> black piece
                         (_, _) -> False

attacks :: Piece -> [Dir] -> Board -> Maybe Move
attacks piece dir board = lookAt pos' board >>= attack
    where pos' = steer (position piece) (colour piece) dir
          attack piece' | opposite (colour piece) piece' = Just $ Take piece piece'
          attack piece' | empty piece' = Just $ Block piece piece'
          attack _ = Nothing

attacksR :: Piece -> [Dir] -> Board -> [Move]
attacksR piece dir board = keep $ unfoldr move piece
    where move p = fmap (\piece' -> ((piece, piece'), piece')) $ lookAt (steer (position p) clr dir) board
          keep ((p, p') : pieces) | empty p'    = (Block p p') : (keep pieces)
          keep ((p, p') : pieces) | opponent p' = (Take  p p') : (keep pieces)
          keep _ = []
          clr  = colour piece
          opponent = opposite (colour piece)

castlesK :: Piece -> Board -> Maybe Move
castlesK piece board = fmap (const castle) $ mfilter (const firstMove) $ mfilter inPlace $ lookAt rookPos board
    where inPlace rook = (rook == eRook) && (piece == eKing)
          rookPos  = if (white piece) then (8, 1) else (8, 8)
          rookPos' = if (white piece) then (6, 1) else (6, 8)
          kingPos  = if (white piece) then (5, 1) else (5, 8)
          kingPos' = if (white piece) then (7, 1) else (7, 8)
          eRook = Rook (colour piece) rookPos
          eKing = King (colour piece) kingPos
          firstMove = isNothing $ find (moved . extract) $ pastMoves board
          moved ((King c p), _) = colour piece == c && p == kingPos
          moved ((Rook c p), _) = colour piece == c && p == rookPos
          moved _ = False
          castle = CastleK (Block eKing (Empty kingPos')) (Block eRook (Empty rookPos'))
    
castlesQ :: Piece -> Board -> Maybe Move
castlesQ piece = fmap (const castle) . mfilter (const firstMove) . mfilter inPlace . lookAt rookPos
    where inPlace rook = (rook == eRook) && (piece == eKing)
          rookPos  = if (white piece) then (1, 1) else (1, 8)
          rookPos' = if (white piece) then (4, 1) else (4, 8)
          kingPos  = if (white piece) then (5, 1) else (5, 8)
          kingPos' = if (white piece) then (3, 1) else (3, 8)
          eRook = Rook (colour piece) rookPos
          eKing = King (colour piece) kingPos
          firstMove = isNothing $ find (moved . extract) $ pastMoves board
          moved ((King c p), _) = colour piece == c && p == kingPos
          moved ((Rook c p), _) = colour piece == c && p == rookPos
          moved _ = False
          castle = CastleQ (Block eKing (Empty kingPos')) (Block eRook (Empty rookPos'))

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

moves :: Piece -> Board -> Set Move
moves piece @ (Pawn _ _)   = S.fromList . pawnMoves piece
moves piece @ (Rook _ _)   = S.fromList . rookMoves piece
moves piece @ (Bishop _ _) = S.fromList . bishopMoves piece
moves piece @ (Knight _ _) = S.fromList . knightMoves piece 
moves piece @ (Queen _ _)  = S.fromList . queenMoves piece
moves piece @ (King _ _)   = S.fromList . kingMoves piece
moves piece @ (Empty _)    = const (S.empty)

threatsFor :: Piece -> Board -> Set Pos
threatsFor piece board = M.foldl gatherThreats S.empty $ M.filter (opposite $ colour piece) $ positions board
        where gatherThreats set piece = S.union set (S.map (position . commit . extract) $ S.filter threats $ moves piece board)
              threats (Take _ _) = True
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
          kingMoves   = S.map (position . commit . extract) $ moves king board
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
adjustKings ((King W _), p) board = board { whiteKing = position p }
adjustKings ((King B _), p) board = board { blackKing = position p }
adjustKings _ board = board

accumulate :: Move -> Board -> Board
accumulate move board = board { pastMoves = move : (pastMoves board) }

apply :: Move -> Board -> Board
apply move @ (CastleK (Block kingS kingE) (Block rookS rookE)) = changeTurn . accumulate move . adjustKings (kingS, kingE) . streamLine [(kingS, kingE), (rookS, rookE)]
apply move @ (CastleQ (Block kingS kingE) (Block rookS rookE)) = changeTurn . accumulate move . adjustKings (kingS, kingE) . streamLine [(kingS, kingE), (rookS, rookE)]
apply move @ (TakeEP piece piece' piece'') = changeTurn . accumulate move . streamLine [(piece, piece'), (piece'', piece'')]
apply move @ (Jump   piece piece')   = changeTurn . accumulate move . streamLine [(piece, piece')]
apply move @ (Take   piece piece')   = changeTurn . accumulate move . adjustKings (piece, piece') . streamLine [(piece, piece')]
apply move @ (Block  piece piece')   = changeTurn . accumulate move . adjustKings (piece, piece') . streamLine [(piece, piece')]

legality :: Piece -> Move -> Board -> Maybe Move
legality p m @ (Take  _ _) board    = (checks p m board) >?= (checked p m board)
legality p m @ (Block _ _) board    = (checks p m board) >?= (checked p m board)
legality p m @ (Jump  _ _) board    = (checks p m board) >?= (checked p m board)
legality p m @ (CastleK _ _) board = (checks p m board) >?= (checked p m board) >?= (kcastle p m board)
legality p m @ (CastleQ _ _) board = (checks p m board) >?= (checked p m board) >?= (qcastle p m board)

move :: Pos -> Pos -> Board -> Board
move pos pos' board = fromMaybe board newBoard
    where newBoard = do
            piece <- mfilter allowed $ lookAt pos board
            move  <- find (available . extract) $ moves piece board
            _     <- legality piece move board
            return (apply move board)
          available (p, p') = (position p, position p') == (pos, pos')
          allowed piece = (colour piece) == (player board)

legal :: Move -> Board -> Bool
legal move board = isJust $ legality (fst $ extract move) move board

--- Do everything over moves
--- A move describes what needs to happen with which pieces
--- The parser later on will have to take care of converting positions and actions to pieces and moves 
--- A move contains the piece that has to move, so I'll also simplify a lot of
move' :: Move -> Board -> Board
move' m board = fromMaybe board $ fmap (\m -> apply m board) $ mfilter (\x -> legal x board) $ find (== m) $ moves piece board
        where piece = fst $ extract m
        
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

time :: IO a -> IO ()
time comp = do
    t0 <- getCPUTime
    let a = unsafePerformIO comp
    t1 <- getCPUTime
    let duration = (fromIntegral (t1 - t0)) / 10^9
    printf "Took: %0.5f msec\n" (duration :: Double)