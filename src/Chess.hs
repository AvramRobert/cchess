module Chess where 

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find, intersperse, unfoldr, any)
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

data Outcome =
    Check     |
    Checkmate |
    Stalemate |
    Draw      |
    Illegal   |
    Continue 
    deriving (Show, Eq, Ord)

data Dir = U | D | L | R

data Board = Board {positions :: Map Pos Piece,
                    pastMoves :: [Move],
                    whiteKing :: Piece,
                    blackKing :: Piece,
                    player    :: Colour}

(~>) :: Outcome -> Outcome -> Outcome
Check     ~> _         = Check
Continue  ~> Check     = Check
Checkmate ~> _         = Checkmate
Continue  ~> Checkmate = Checkmate
Stalemate ~> _         = Stalemate
Continue  ~> Stalemate = Stalemate
Draw      ~> _         = Draw
Continue  ~> Draw      = Draw
Illegal   ~> _         = Illegal
Continue  ~> Illegal   = Illegal
outcome   ~> _ = outcome

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
commit ((Knight c _), p) = Knight c $ position p

perform :: Move -> Piece
perform (Take p p')     = commit (p, p')
perform (Block p p')    = commit (p, p')
perform (Jump p p')     = commit (p, p')
perform (TakeEP p p' _) = commit (p, p')
perform (CastleK m _)   = perform m
perform (CastleQ m _)   = perform m
perform (Promote p p')  = p'

piece :: Move -> Piece
piece (Take p _)     = p
piece (Block p _)    = p
piece (Jump p _)     = p
piece (TakeEP p _ _) = p
piece (Promote p _)  = p
piece (CastleK m _) = piece m
piece (CastleQ m _) = piece m

currentKing :: Board -> Piece
currentKing (Board _ _ wk _ W) = wk
currentKing (Board _ _ _ bk B) = bk

piecesWhere :: (Piece -> Bool) -> Board -> [Piece]
piecesWhere p = M.elems . M.filter p . positions

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

isAt :: Pos -> (Piece -> Bool) -> Board -> Bool
isAt pos p (Board ps _ _ _ _) = fromMaybe False $ fmap p $ M.lookup pos ps

at :: [Dir] -> Piece -> Board -> Maybe Piece
at dir piece board = lookAt (steer pos clr dir) board
    where pos = position piece
          clr = case (colour piece) of T -> player board
                                       _ -> colour piece

enpassant :: Piece -> [Dir] -> Board -> Maybe Move
enpassant piece dir board = fmap take $ mfilter passant $ fmap perform $ mfilter jump $ maybeFirst $ pastMoves board
    where maybeFirst (x : xs) = Just x
          maybeFirst _ = Nothing
          jump (Jump _ _) = True
          jump _          = False
          passant (Pawn c (x, y)) = (c /= (colour piece)) && (y == (snd $ position piece)) && isAt pos empty board
          pos = steer (position piece) (colour piece) dir
          take = TakeEP piece (Empty pos)

promote :: Piece -> Piece -> Board -> Maybe Move
promote piece @ (Pawn W (_, 8)) piece' board = Just (Promote piece piece') 
promote piece @ (Pawn B (_, 1)) piece' board = Just (Promote piece piece')
promote _ _ _ = Nothing

takes :: Piece -> [Dir] -> Board -> Maybe Move
takes piece dir = fmap (Take piece) . mfilter (opposite (colour piece)) . at dir piece

blocks :: Piece -> [Dir] -> Board -> Maybe Move
blocks piece dir = fmap (Block piece) . mfilter empty . at dir piece

jumps :: Piece -> [Dir] -> Board -> Maybe Move
jumps piece dir = fmap (Jump piece) . mfilter empty . mfilter (const atStart) . at dir piece
    where atStart = case (position piece) of 
                         (x, 2) -> white piece
                         (x, 7) -> black piece
                         (_, _) -> False

attacks :: Piece -> [Dir] -> Board -> Maybe Move
attacks piece dir board = attack =<< at dir piece board 
    where attack piece' | opponent piece' = Just $ Take piece piece'
          attack piece' | empty piece' = Just $ Block piece piece'
          attack _ = Nothing
          opponent = opposite (colour piece)

attacksR :: Piece -> [Dir] -> Board -> [Move]
attacksR piece dir board = unfoldr move piece
    where move p = keep =<< at dir p board
          keep piece' | empty piece'    = Just (Block piece piece', piece')
          keep piece' | opponent piece' = Just (Take  piece piece', piece')
          keep _ = Nothing
          opponent = opposite (colour piece)

castlesK :: Piece -> Board -> Maybe Move
castlesK p board = fmap castle $ mfilter (const firstMove) $ mfilter inPlace $ mzip (lookAt kingPos board) (lookAt rookPos board)
    where inPlace (king, rook) = (rook == rookS) && (king == kingS)
          rookPos  = if (white p) then (8, 1) else (8, 8)
          rookPos' = if (white p) then (6, 1) else (6, 8)
          kingPos  = if (white p) then (5, 1) else (5, 8)
          kingPos' = if (white p) then (7, 1) else (7, 8)
          rookS = Rook (colour p) rookPos
          kingS = King (colour p) kingPos
          firstMove = isNothing $ find (moved . piece) $ pastMoves board
          moved piece = (piece == rookS) || (piece == kingS)
          castle (king, rook) = CastleK (Block king (Empty kingPos')) (Block rook (Empty rookPos'))

castlesQ :: Piece -> Board -> Maybe Move
castlesQ p board = fmap castle $ mfilter (const firstMove) $ mfilter inPlace $ mzip (lookAt kingPos board) (lookAt rookPos board)
    where inPlace (king, rook) = (rook == rookS) && (king == kingS)
          rookPos  = if (white p) then (1, 1) else (1, 8)
          rookPos' = if (white p) then (4, 1) else (4, 8)
          kingPos  = if (white p) then (5, 1) else (5, 8)
          kingPos' = if (white p) then (3, 1) else (3, 8)
          rookS = Rook (colour p) rookPos
          kingS = King (colour p) kingPos
          firstMove = isNothing $ find (moved . piece) $ pastMoves board
          moved piece = (piece == rookS) || (piece == kingS)
          castle (king, rook) = CastleQ (Block king (Empty kingPos')) (Block rook (Empty rookPos'))

pawnMoves :: Piece -> Board -> [Move]
pawnMoves pawn board = catMaybes [promote pawn (Queen c p) board,
                                  promote pawn (Bishop c p) board,
                                  promote pawn (Knight c p) board,
                                  promote pawn (Rook c p) board,
                                  enpassant pawn [R, D] board,
                                  enpassant pawn [L, D] board,
                                  takes pawn  [L, U] board,
                                  takes pawn  [R, U] board,
                                  blocks pawn [U] board,
                                  jumps pawn  [U, U] board]
        where c = colour pawn
              p = position pawn

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

threats :: Board -> Set Pos
threats board = M.foldl gather S.empty $ M.filter (opposite $ player board) $ positions board
            where gather set piece = S.union set (S.map (position . perform) $ S.filter threat $ moves piece board)
                  threat (Take _ _) = True
                  threat (Block _ _) = True
                  threat (TakeEP _ _ _) = True
                  threat _ = False

check :: Move -> Board -> Outcome 
check move board = if inCheck then Check else Continue
    where king = currentKing board
          inCheck = S.member (position king) $ threats board

checkmate :: Move -> Board -> Outcome
checkmate move board  = if inCheckmate then Checkmate else Continue
    where king        = currentKing board
          inCheckmate = inCheck && cannotMove && unblockable
          inCheck     = S.member (position king) allThreats
          cannotMove  = S.isSubsetOf kingMoves allThreats
          unblockable = (check move $ apply move board) /= Check 
          kingMoves   = S.map (position . perform) $ moves king board
          allThreats  = threats board

turn :: Move -> Board -> Outcome
turn move board = if ((colour $ piece move) == player board)
                  then Continue
                  else Illegal

available :: Move -> Board -> Outcome
available move board = fromMaybe Illegal $ fmap (const Continue) $ find (== move) $ moves (piece move) board

checked :: Piece -> Move -> Board -> Outcome
checked piece move = check move . apply move 

qcastle :: Move -> Board -> Outcome
qcastle move board = if free then Continue else Illegal
    where free = S.null $ S.intersection safetyPath $ threats board
          safetyPath = S.fromList files
          files = if (W == (player board))
                  then [(2, 1), (3, 1), (4, 1)]
                  else [(2, 8), (3, 8), (4, 8)]

kcastle :: Move -> Board -> Outcome
kcastle move board = if free then Continue else Illegal
    where free = S.null $ S.intersection safetyPath $ threats board
          safetyPath = S.fromList files
          files = if (W == (player board)) 
                  then [(5, 1), (6, 1), (7, 1)]
                  else [(5, 8), (6, 8), (7, 8)]   

{- 
Rules of drawing:
            1. Stalemate: Not in check, but has no legal move
            2. Threefold repetition of a position: 
                    -> If it's the same player's turn to move, and there have been 3 positions throughout the game where he was in the exact same position,
                       with exactly the same possible moves => he can claim a draw (not automatic)

            3. 50 move rule (not automatic): No captures or pawns moved in the last 50 moves
            4. Impossible checkmates:
                    -> King vs King
                    -> King Bishop vs King

                    -> King Kinght vs King
                    -> King Bishop vs King Bishop (where bishops have the same colour)
            5. Mutual agreement. 

-}

stalemate :: Board -> Outcome
stalemate board = if (immovable && (not inCheck) && movedBefore) then Stalemate else Continue
            where immovable = S.null $ moves king board 
                  inCheck   = S.member (position king) allThreats
                  movedBefore = any (movedKing . perform) $ pastMoves board
                  king = currentKing board
                  allThreats = threats board
                  movedKing (King c _) = c == (colour king)
                  movedKing _ = False 

fiftyMove :: Board -> Outcome
fiftyMove board = if (length $ pastMoves board) > 50 then illegality else Continue
        where illegality = fromMaybe Illegal $ fmap (const Continue) $ find takeOrPawn $ take 50 $ pastMoves board
              takeOrPawn (Take _ _) = True
              takeOrPawn (TakeEP _ _ _) = True
              takeOrPawn (Block (Pawn _ _) _) = True
              takeOrPawn _ = False

kingVsKing :: Board -> Outcome
kingVsKing board = fromMaybe Continue $ fmap (const Illegal) $ mfilter onlyTwo $ mzip (lookAt whiteKingPos board) (lookAt blackKingPos board)
            where onlyTwo _ = (M.size $ positions board) == 2
                  whiteKingPos = position $ whiteKing board
                  blackKingPos = position $ blackKing board

kingBishopVsKing :: Board -> Outcome
kingBishopVsKing board = if (justThree && wking && bking && bking) then Draw else Continue
            where justThree = (M.size $ positions board) == 3
                  pieces = M.elems $ positions board
                  bishop = any (\x -> case x of (Bishop _ _) -> True
                                                _            -> False) pieces
                  wking  = any (== (whiteKing board)) pieces
                  bking  = any (== (blackKing board)) pieces

kingKnightVsKing :: Board -> Outcome
kingKnightVsKing board = if (justThree && wking && bking && knight) then Draw else Continue
            where justThree = (M.size $ positions board) == 3
                  pieces = M.elems $ positions board
                  knight = any (\x -> case x of (Knight _ _) -> True
                                                _            -> False) pieces
                  wking  = any (== (whiteKing board)) pieces
                  bking  = any (== (blackKing board)) pieces

-- How?
threeFold :: Board -> Outcome
threeFold _ = Continue

kingBishopVsKingBishop :: Board -> Outcome
kingBishopVsKingBishop board = if (justFour && wking && bking && sameBishops) then Draw else Continue
            where justFour = (M.size $ positions board) == 4
                  pieces = M.elems $ positions board
                  bishops = filter (\x -> case x of (Bishop _ _) -> True
                                                    _            -> False) pieces
                  sameBishops = (length bishops) == 2 && (all whiteSquare bishops || all blackSquare bishops)                                   
                  whiteSquare (Bishop _ (x, y)) = (odd x && even y) || (even x && odd y)
                  blackSquare (Bishop _ (x, y)) = (odd x && odd y)  || (even x && even y)
                  wking = any (== (whiteKing board)) pieces
                  bking = any (== (blackKing board)) pieces

checkmateless :: Board -> Outcome
checkmateless board = kingVsKing board ~> kingBishopVsKing board ~> kingKnightVsKing board ~> kingBishopVsKingBishop board

draw :: Board -> Outcome
draw board = stalemate board ~> fiftyMove board ~> checkmateless board

changeTurn :: Board -> Board
changeTurn board = board { player = invert $ player board }

adjustKings :: Piece -> Board -> Board
adjustKings k @ (King W _) board = board { whiteKing = k }
adjustKings k @ (King B _) board = board { blackKing = k }
adjustKings _ board = board 

accumulate :: Move -> Board -> Board
accumulate move board = board { pastMoves = move : (pastMoves board) }

add :: Piece -> Board -> Board
add piece board = board { positions = M.insert (position piece) piece $ positions board }

remove :: Piece -> Board -> Board
remove piece board = board { positions = M.insert pos (Empty pos) $ positions board }
            where pos = position piece

apply :: Move -> Board -> Board
apply move @ (CastleK m1 @ (Block king _) m2 @ (Block rook _)) = changeTurn . accumulate move . adjustKings (perform m1) . add (perform m1) . add (perform m2) . remove king . remove rook
apply move @ (CastleQ m1 @ (Block king _) m2 @ (Block rook _)) = changeTurn . accumulate move . adjustKings (perform m1) . add (perform m1) . add (perform m2) . remove king . remove rook
apply move @ (TakeEP  piece piece' piece'') = changeTurn . accumulate move . add (perform move) . remove piece''
apply move @ (Jump    piece piece')   = changeTurn . accumulate move . add (perform move) . remove piece
apply move @ (Take    piece piece')   = changeTurn . accumulate move . adjustKings (perform move) . add (perform move) . remove piece
apply move @ (Block   piece piece')   = changeTurn . accumulate move . adjustKings (perform move) . add (perform move) . remove piece
apply move @ (Promote piece piece')   = changeTurn . accumulate move . add (perform move) . remove piece

legality :: Move -> Board -> Outcome
legality m @ (Take  p _)   board = (turn m board) ~> (available m board) ~> (check m board) ~> (checkmate m board) ~> (checked p m board) ~> (draw board)
legality m @ (Block p _)   board = (turn m board) ~> (available m board) ~> (check m board) ~> (checkmate m board) ~> (checked p m board) ~> (draw board)
legality m @ (Jump  p _)   board = (turn m board) ~> (available m board) ~> (check m board) ~> (checkmate m board) ~> (checked p m board) ~> (draw board)
legality m @ (Promote p _) board = (turn m board) ~> (available m board) ~> (check m board) ~> (checkmate m board) ~> (checked p m board) ~> (draw board)
legality m @ (CastleK _ _) board = (turn m board) ~> (available m board) ~> (check m board) ~> (checkmate m board) ~> (kcastle m board) ~> (draw board)
legality m @ (CastleQ _ _) board = (turn m board) ~> (available m board) ~> (check m board) ~> (checkmate m board) ~> (qcastle m board) ~> (draw board)

move :: Move -> Board -> (Outcome, Board)
move m board = case (legality m board) of 
               Continue -> (Continue, apply m board)
               outcome  -> (outcome, board)
        
board :: Board 
board = Board { positions = M.fromList positions,
                pastMoves = [],
                whiteKing = (King W (5, 1)), 
                blackKing = (King B (5, 8)),
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

--- Idea:
--- a) Do transformations in just one coordinate orientation and define an `invert` function that just inverts the values at the end for the respective colour
--- x b) Concretise moves: Take, Block, TakeEp -> Piece Pos. Make only one Castle case but with a different data Side = Q | K
--- c) Use board colour, not piece colour
--- d) Steer directions based on the board, not the piece