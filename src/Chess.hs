module Chess where 

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find, intersperse, any)
import Control.Monad (mfilter)
import Control.Monad.Zip (mzip)
import Control.Applicative ((<|>))
import Data.Maybe (catMaybes, fromMaybe, isNothing)

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


instance Show Board where
    show board = unlines $ fmap row [1..8]
        where row y = foldl (++) "" $ intersperse "," $ catMaybes $ fmap (\x -> fmap show $ lookAt (x, y) board) [1..8]

kingCastleRook :: Colour -> Piece
kingCastleRook W = Rook W (8, 1)
kingCastleRook B = Rook B (8, 8)

queenCastleRook :: Colour -> Piece
queenCastleRook W = Rook W (1, 1)
queenCastleRook B = Rook W (1, 8)

kingCastle :: Colour -> Piece
kingCastle W = King W (5, 1)
kingCastle B = King B (5, 8)

kingSideCastle :: Colour -> Move
kingSideCastle W = CastleK (Block (kingCastle W) (Empty (7, 1))) (Block (kingCastleRook W) (Empty (6, 1)))
kingSideCastle B = CastleK (Block (kingCastle B) (Empty (7, 8))) (Block (kingCastleRook B) (Empty (6, 8)))

queenSideCastle :: Colour -> Move
queenSideCastle W = CastleQ (Block (kingCastle W) (Empty (3, 1))) (Block (queenCastleRook W) (Empty (4, 1)))
queenSideCastle B = CastleQ (Block (kingCastle B) (Empty (3, 8))) (Block (queenCastleRook B) (Empty (4, 8)))

figure :: Piece -> String 
figure (Pawn W p)   = "♙"
figure (Pawn B _)   = "♟"
figure (Rook W _)   = "♖"
figure (Rook B _)   = "♜"
figure (Bishop W _) = "♗"
figure (Bishop B _) = "♝"
figure (Knight W _) = "♘"
figure (Knight B _) = "♞"
figure (Queen W _)  = "♕"
figure (Queen B _)  = "♛"
figure (King W _)   = "♔"
figure (King B _)   = "♚"
figure (Empty _)    = " "

instance Show Piece where
    show piece = "[ " <> (figure piece) <> " " <> (show $ position piece) <> " ]"
    
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

commit :: Piece -> Piece -> Piece
commit (Pawn c _)   = Pawn c . position
commit (King c _)   = King c . position
commit (Rook c _)   = Rook c . position
commit (Queen c _)  = Queen c . position 
commit (Bishop c _) = Bishop c . position
commit (Knight c _) = Knight c . position

perform :: Move -> Piece
perform (Take p p')     = commit p p'
perform (Block p p')    = commit p p'
perform (Jump p p')     = commit p p'
perform (TakeEP p p' _) = commit p p'
perform (CastleK m _)   = perform m
perform (CastleQ m _)   = perform m
perform (Promote p p')  = p'

pieceFrom :: Move -> Piece
pieceFrom (Take p _)     = p
pieceFrom (Block p _)    = p
pieceFrom (Jump p _)     = p
pieceFrom (TakeEP p _ _) = p
pieceFrom (Promote p _)  = p
pieceFrom (CastleK m _)  = pieceFrom m
pieceFrom (CastleQ m _)  = pieceFrom m

currentKing :: Board -> Piece
currentKing (Board _ _ wk _ W) = wk
currentKing (Board _ _ _ bk B) = bk

black :: Piece -> Bool
black = (== B) . colour

white :: Piece -> Bool
white = (== W) . colour

empty :: Piece -> Bool
empty (Empty _) = True
empty _         = False

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

lookInto :: [Dir] -> Piece -> Board -> Maybe Piece
lookInto dir piece = lookAt (steer pos clr dir)
    where pos = position piece
          clr = colour piece

enpassant :: [Dir] -> Piece -> Board -> Maybe Move
enpassant dir piece board = fmap take $ mfilter movable $ mzip newSquare $ mfilter isPassant $ fmap perform $ mfilter isJump $ first $ pastMoves board
    where (x, y)                     = position piece
          player                     = colour piece
          newSquare                  = lookInto dir piece board
          first (x : xs)             = Just x
          first _                    = Nothing
          isJump (Jump _ _)          = True
          isJump _                   = False
          isPassant (Pawn c (_, y')) = c /= player && (y == y')
          movable (Empty pos, piece) = True
          movable _                  = False 
          take (e, p)                = TakeEP piece e p

promote :: Piece -> Piece -> Board -> Maybe Move
promote piece @ (Pawn W (_, 8)) piece' board = Just (Promote piece piece') 
promote piece @ (Pawn B (_, 1)) piece' board = Just (Promote piece piece')
promote _ _ _ = Nothing

capture :: [Dir] -> Piece -> Board -> Maybe Move
capture dir piece = fmap (Take piece) . mfilter (opposite $ colour piece) . lookInto dir piece

block :: [Dir] -> Piece -> Board -> Maybe Move
block dir piece = fmap (Block piece) . mfilter empty . lookInto dir piece

jump :: [Dir] -> Piece -> Board -> Maybe Move
jump dir piece = fmap (Jump piece) . mfilter empty . mfilter (const atStart) . lookInto dir piece
    where atStart = case (position piece) of 
                         (x, 2) -> white piece
                         (x, 7) -> black piece
                         (_, _) -> False

attack :: [Dir] -> Piece -> Board -> Maybe Move
attack dir piece board = (block dir piece board) <|> (capture dir piece board) 

attacking :: [Dir] -> Piece  -> Board -> [Move]
attacking dir piece board = gather $ attack dir piece board
        where gather (Just m @ (Block _ piece')) = (Block piece piece') : (gather $ attack dir (perform m) board)
              gather (Just m @ (Take  _ piece')) = (Take piece piece') : (gather Nothing)
              gather (Nothing)                   = []

castlesK :: Piece -> Board -> Maybe Move
castlesK piece board = fmap (const castle) $ mfilter (const firstMove) $ mfilter inPlace $ mzip boardKing boardRook
        where boardKing        = lookAt (position king) board
              boardRook        = lookAt (position rook) board
              king             = kingCastle player
              rook             = kingCastleRook player
              player           = colour piece
              inPlace (bk, br) = bk == king && br == rook
              firstMove        = any (not . rookOrKing . pieceFrom) $ pastMoves board
              rookOrKing p     = (p == rook) || (p == king)
              castle           = kingSideCastle player 

castlesQ :: Piece -> Board -> Maybe Move
castlesQ piece board = fmap (const castle) $ mfilter (const firstMove) $ mfilter inPlace $ mzip boardKing boardRook
        where boardKing        = lookAt (position king) board
              boardRook        = lookAt (position rook) board
              king             = kingCastle player
              rook             = queenCastleRook player
              player           = colour piece
              inPlace (bk, br) = bk == king && br == rook
              firstMove        = any (not . rookOrKing . pieceFrom) $ pastMoves board
              rookOrKing p     = (p == rook) || (p == king)
              castle           = queenSideCastle player 

pawnMoves :: Piece -> Board -> [Move]
pawnMoves pawn board = catMaybes [promote pawn (Queen c p) board,
                                  promote pawn (Bishop c p) board,
                                  promote pawn (Knight c p) board,
                                  promote pawn (Rook c p) board,
                                  block [U] pawn board,
                                  enpassant [R, U] pawn board,
                                  enpassant [L, U] pawn board,
                                  capture [L, U] pawn board,
                                  capture [R, U] pawn board,
                                  jump [U, U] pawn board]
        where c = colour pawn
              p = position pawn

kingMoves :: Piece -> Board -> [Move]
kingMoves king board = catMaybes [attack [L, U] king board, 
                                  attack [U, R] king board,
                                  attack [D, R] king board,
                                  attack [D, L] king board,
                                  attack [U] king board,
                                  attack [R] king board,
                                  attack [D] king board,
                                  attack [L] king board,
                                  castlesK king board,
                                  castlesQ king board]

rookMoves :: Piece -> Board -> [Move]
rookMoves rook board = (attacking [U] rook board) ++ 
                       (attacking [L] rook board) ++ 
                       (attacking [R] rook board) ++
                       (attacking [D] rook board)

                    
bishopMoves :: Piece -> Board -> [Move]
bishopMoves bishop board = (attacking [L, U] bishop board) ++
                           (attacking [R, U] bishop board) ++
                           (attacking [L, D] bishop board) ++
                           (attacking [R, D] bishop board)

queenMoves :: Piece -> Board -> [Move]
queenMoves queen board = (rookMoves queen board) ++ (bishopMoves queen board)

knightMoves :: Piece -> Board -> [Move]
knightMoves knight board = catMaybes [attack [U, U, R] knight board,
                                      attack [U, U, L] knight board,
                                      attack [D, D, R] knight board,
                                      attack [D, D, L] knight board,
                                      attack [U, L, L] knight board,
                                      attack [U, R, R] knight board,
                                      attack [D, L, L] knight board,
                                      attack [D, R, R] knight board]

moves :: Piece -> Board -> Set Move
moves piece @ (Pawn _ _)   = S.fromList . pawnMoves piece
moves piece @ (Rook _ _)   = S.fromList . rookMoves piece
moves piece @ (Bishop _ _) = S.fromList . bishopMoves piece
moves piece @ (Knight _ _) = S.fromList . knightMoves piece 
moves piece @ (Queen _ _)  = S.fromList . queenMoves piece
moves piece @ (King _ _)   = S.fromList . kingMoves piece
moves piece @ (Empty _)    = const (S.empty)

-- Here I have to fictively remap the board 
threats :: Board -> Set Pos
threats board = M.foldl gather S.empty $ M.filter (opposite $ player board) $ positions board
            where gather set piece = S.union set (S.map (position . perform) $ S.filter threat $ moves piece board')
                  board' = changeTurn board
                  threat (Take _ _) = True
                  threat (Block _ _) = True
                  threat (TakeEP _ _ _) = True
                  threat _ = False

checked :: Board -> Bool
checked board = S.member (position king) $ threats board
        where king = currentKing board

isCheck :: Move -> Board -> Bool 
isCheck move board  = S.member (position king) $ threats board'
    where board'  = attempt move board
          king    = currentKing board'

isCheckmate :: Board -> Bool
isCheckmate board = all inCheck $ availableMoves board
        where inCheck m = isCheck m board

checks :: Move -> Board -> Outcome
checks move board = case (isCheck move board) of
            True | isCheckmate board -> Checkmate
            True -> Check
            _    -> Continue 

turn :: Move -> Board -> Outcome
turn move board = if ((colour $ pieceFrom move) == player board)
                  then Continue
                  else Illegal

available :: Move -> Board -> Outcome
available move board = fromMaybe Illegal $ fmap (const Continue) $ find (== move) $ moves (pieceFrom move) board

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

attempt :: Move -> Board -> Board
attempt move @ (CastleK m1 @ (Block king _) m2 @ (Block rook _)) = adjustKings (perform m1) . add (perform m1) . add (perform m2) . remove king . remove rook
attempt move @ (CastleQ m1 @ (Block king _) m2 @ (Block rook _)) = adjustKings (perform m1) . add (perform m1) . add (perform m2) . remove king . remove rook
attempt move @ (TakeEP  piece piece' piece'') = add (perform move) . remove piece'' . remove piece
attempt move @ (Jump    piece piece')   = add (perform move) . remove piece
attempt move @ (Take    piece piece')   = adjustKings (perform move) . add (perform move) . remove piece
attempt move @ (Block   piece piece')   = adjustKings (perform move) . add (perform move) . remove piece
attempt move @ (Promote piece piece')   = add (perform move) . remove piece

apply :: Move -> Board -> Board
apply move = changeTurn . accumulate move . attempt move

legality :: Move -> Board -> Outcome
legality m @ (Take _ _)     board = (turn m board) ~> (available m board) ~> (checks m board) ~> (draw board)
legality m @ (TakeEP _ _ _) board = (turn m board) ~> (available m board) ~> (checks m board) ~> (draw board)
legality m @ (Block _ _)    board = (turn m board) ~> (available m board) ~> (checks m board) ~> (draw board)
legality m @ (Jump _ _)     board = (turn m board) ~> (available m board) ~> (checks m board) ~> (draw board)
legality m @ (Promote _ _)  board = (turn m board) ~> (available m board) ~> (checks m board) ~> (draw board)
legality m @ (CastleK _ _)  board = (turn m board) ~> (available m board) ~> (checks m board) ~> (kcastle m board) ~> (draw board)
legality m @ (CastleQ _ _)  board = (turn m board) ~> (available m board) ~> (checks m board) ~> (qcastle m board) ~> (draw board)

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

---- API ----

availableMoves :: Board -> Set Move
availableMoves board = foldl gather S.empty $ M.elems $ M.filter currentPlayer $ positions board
    where gather set piece = set <> (moves piece board)
          currentPlayer p = (colour p) == (player board)
           
movesFor :: (Piece -> Bool) -> Board -> Set Move
movesFor p board = foldl gather S.empty $ M.elems $ M.filter p $ positions board
    where gather set piece = set <> (moves piece board)

-- Change `at` to use the piece colour, not the board colour` (alawys move the piece on the board and recur. That should keep the colour)
-- Move the path castles check to the castles move generator
-- Rethink the castles algebraic case. It needn't be CastleK Move Move. It can be Castle King

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