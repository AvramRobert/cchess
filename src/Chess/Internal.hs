module Chess.Internal where 

import Data.Map (Map)
import Data.Set (Set)
import Data.Either (Either, isRight)
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

-- Instead of making the moves concrete chess moves, I can describe them in terms of what actions may be applied on the board and let them be a [Action]
-- For example the most primitive actions are removing, placing, replacing (or capture, block, promote) => takeEnpassant :: [Action]
-- The only question is, how do I think about threats.
-- Well, instead of deciding based on the ADT, I can actually perform the move and see what effect it has on the board.

data Move =
    Take    Piece Piece           |
    Block   Piece Piece           |
    Jump    Piece Piece           |
    TakeEP  Piece Piece Piece     |
    TakePromote Piece Piece Piece |
    Promote Piece Piece           |
    CastleK Move Move             |
    CastleQ Move Move
    deriving (Show, Eq, Ord)

data Outcome =
    Check     |
    Checkmate |
    Stalemate |
    Draw      |
    Illegal   
    deriving (Show, Eq, Ord)

data Dir = U | D | L | R

data Board = Board {positions :: Map Pos Piece,
                    pastMoves :: [Move],
                    whiteKing :: Piece,
                    blackKing :: Piece,
                    player    :: Colour}


instance Show Piece where
    show piece = "[ " <> (figure piece) <> " " <> (show $ position piece) <> " ]"

instance Show Board where
    show board = unlines $ reverse $ fmap row [1..8]
        where row y = foldl (++) "" $ intersperse "," $ catMaybes $ fmap (\x -> fmap show $ lookAt (x, y) board) [1..8]

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
        
whiteSquare :: Pos -> Bool
whiteSquare (x, y) = (odd x && even y) || (even x && odd y)

blackSquare :: Pos -> Bool
blackSquare (x, y) = (odd x && odd y)  || (even x && even y) 

rookKing :: Colour -> Piece
rookKing W = Rook W (8, 1)
rookKing B = Rook B (8, 8)

rookQueen :: Colour -> Piece
rookQueen W = Rook W (1, 1)
rookQueen B = Rook B (1, 8)

king :: Colour -> Piece
king W = King W (5, 1)
king B = King B (5, 8)

kingSideCastle :: Colour -> Move
kingSideCastle W = CastleK (Block (king W) (Empty (7, 1))) (Block (rookKing W) (Empty (6, 1)))
kingSideCastle B = CastleK (Block (king B) (Empty (7, 8))) (Block (rookKing B) (Empty (6, 8)))

queenSideCastle :: Colour -> Move
queenSideCastle W = CastleQ (Block (king W) (Empty (3, 1))) (Block (rookQueen W) (Empty (4, 1)))
queenSideCastle B = CastleQ (Block (king B) (Empty (3, 8))) (Block (rookQueen B) (Empty (4, 8)))

kingSideFiles :: Colour -> Set Pos
kingSideFiles W = S.fromList $ [(6, 1), (7, 1)]
kingSideFiles B = S.fromList $ [(6, 8), (7, 8)]

queenSideFiles :: Colour -> Set Pos
queenSideFiles W = S.fromList $ [(3, 1), (4, 1)]
queenSideFiles B = S.fromList $ [(3, 8), (4, 8)]

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
perform (Take p p')          = commit p p'
perform (Block p p')         = commit p p'
perform (Jump p p')          = commit p p'
perform (TakeEP p p' _)      = commit p p'
perform (TakePromote p _ p')  = p'
perform (CastleK m _)        = perform m
perform (CastleQ m _)        = perform m
perform (Promote p p')       = p'

pieceFrom :: Move -> Piece
pieceFrom (Take p _)          = p
pieceFrom (Block p _)         = p
pieceFrom (Jump p _)          = p
pieceFrom (TakeEP p _ _)      = p
pieceFrom (TakePromote p _ _) = p
pieceFrom (Promote p _)       = p
pieceFrom (CastleK m _)       = pieceFrom m
pieceFrom (CastleQ m _)       = pieceFrom m

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

promote :: Piece -> (Colour -> Pos -> Piece) -> Board -> Maybe Move
promote piece f board = case piece of (Pawn W (x, 7)) -> promoteTo (f W (x, 8)) (x, 8)
                                      (Pawn B (x, 2)) -> promoteTo (f B (x, 1)) (x, 1) 
                                      _               -> Nothing
        where promoteTo piece' pos = fmap (const (Promote piece piece')) $ mfilter empty $ lookAt pos board

takePromote :: Piece -> [Dir] -> (Colour -> Pos -> Piece) -> Board -> Maybe Move
takePromote piece dir f board = case piece of (Pawn W (x, 7)) -> fmap (promoteTo W) $ capture dir piece board 
                                              (Pawn B (x, 2)) -> fmap (promoteTo B) $ capture dir piece board
                                              _               -> Nothing
            where promoteTo colour (Take p p') = TakePromote p p' (f colour (position p'))

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

castle :: Piece -> Piece -> Move -> Board -> Maybe Move
castle king rook move board = fmap (const move) $ mfilter (const firstMove) $ mfilter inPlace $ mzip boardKing boardRook 
        where boardKing        = lookAt (position king) board
              boardRook        = lookAt (position rook) board
              firstMove        = any (not . rookOrKing . pieceFrom) $ pastMoves board
              inPlace (k, r)   = k == king && r == rook
              rookOrKing p     = p == king || p == rook 
    
castleKingSide :: Piece -> Board -> Maybe Move
castleKingSide piece = castle (king player) (rookKing player) (kingSideCastle player)
        where player = colour piece
              
castleQueenSide :: Piece -> Board -> Maybe Move
castleQueenSide piece = castle (king player) (rookQueen player) (queenSideCastle player)
        where player  = colour piece

pawnMoves :: Piece -> Board -> [Move]
pawnMoves pawn board = catMaybes [promote pawn Queen board,
                                  promote pawn Bishop board,
                                  promote pawn Knight board,
                                  promote pawn Rook board,
                                  takePromote pawn [L, U] Queen board,
                                  takePromote pawn [R, U] Queen board,
                                  takePromote pawn [L, U] Bishop board,
                                  takePromote pawn [R, U] Bishop board,
                                  takePromote pawn [L, U] Knight board,
                                  takePromote pawn [R, U] Knight board,
                                  takePromote pawn [L, U] Rook board,
                                  takePromote pawn [R, U] Rook board,
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
                                  castleKingSide king board,
                                  castleQueenSide king board]

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

threats :: Board -> Set Pos
threats board = M.foldl gather S.empty $ M.filter (opposite $ player board) $ positions board
            where gather set piece = S.union set (S.map (position . perform) $ S.filter threat $ moves piece board)
                  threat (Take _ _)          = True
                  threat (Block _ _)         = True
                  threat (TakeEP _ _ _)      = True
                  threat (TakePromote _ _ _) = True
                  threat _                   = False

checked :: Board -> Bool
checked board = S.member (position king) $ threats board
        where king = currentKing board

isCheck :: Move -> Board -> Bool 
isCheck move = checked . attempt move

isCheckmate :: Board -> Bool
isCheckmate board = all inCheck $ allMoves board
        where inCheck m = isCheck m board

checks :: Move -> Board -> Either Outcome Board
checks move board = case (isCheck move board) of
            True | isCheckmate board -> Left Checkmate
            True                     -> Left Check
            _                        -> Right board 

turn :: Move -> Board -> Either Outcome Board
turn move board = if ((colour $ pieceFrom move) == player board)
                  then Right board
                  else Left Illegal

ruleCheck :: Outcome -> [(Board -> [Piece] -> Bool)] -> Board -> Either Outcome Board
ruleCheck outcome ps board = if anyRule then Left outcome else Right board
    where pieces  = M.elems $ positions board
          anyRule = any (\p -> p board pieces) ps

available :: Move -> Board -> Either Outcome Board
available move board = fromMaybe (Left Illegal) $ fmap (const (Right board)) $ find (== move) $ moves (pieceFrom move) board

castleFiles :: Set Pos -> Board -> Either Outcome Board
castleFiles path board = if free then (Right board) else (Left Illegal)
    where free = S.null $ S.intersection path $ threats board 

castles :: Move -> Board -> Either Outcome Board
castles (CastleK _ _) board = castleFiles (kingSideFiles $ player board) board
castles (CastleQ _ _) board = castleFiles (queenSideFiles $ player board) board

stalemate :: Board -> Either Outcome Board
stalemate board = if (noLegalMoves && notInCheck) then Left Stalemate else Right board
            where notInCheck   = not $ checked board
                  noLegalMoves = S.null $ allMoves board

-- Fifty move rule means that a player can offer a draw.
-- It doesn't mean that it is automatically forced to a draw or automatically
-- I have to think about how I might introduce these "suggestions"
fiftyMove :: Board -> Either Outcome Board
fiftyMove board = if (length $ pastMoves board) > 50 then illegal else legal
        where illegal = fromMaybe (Left Draw) $ fmap (const legal) $ find takeOrPawn $ take 50 $ pastMoves board
              legal                           = Right board
              takeOrPawn (Take _ _)           = True
              takeOrPawn (TakeEP _ _ _)       = True
              takeOrPawn (TakePromote _ _ _)  = True
              takeOrPawn (Block (Pawn _ _) _) = True
              takeOrPawn _                    = False

kingBishopVsKingBishop :: Board -> [Piece] -> Bool
kingBishopVsKingBishop board pieces = fourPieces && bothKings && twoBishops && sameBishops
        where bishops     = filter (\x -> case x of (Bishop _ _) -> True; _ -> False) pieces
              fourPieces  = length pieces == 4
              twoBishops  = length bishops == 2
              bothKings   = any (== (whiteKing board)) pieces && any (== (blackKing board)) pieces
              sameBishops = all (whiteSquare . position) bishops || all (blackSquare . position) bishops

kingKnightVsKing :: Board -> [Piece] -> Bool
kingKnightVsKing board pieces = threePieces && bothKings && anyKnight
        where threePieces = length pieces == 3
              bothKings   = any (== (whiteKing board)) pieces && any (== (blackKing board)) pieces
              anyKnight   = any (\x -> case x of (Knight _ _) -> True; _ -> False) pieces

kingBishopVsKing :: Board -> [Piece] -> Bool
kingBishopVsKing board pieces = threePieces && bothKings && anyBishop
        where threePieces = length pieces == 3
              bothKings   = any (== (whiteKing board)) pieces && any (== (blackKing board)) pieces
              anyBishop   = any (\x -> case x of (Bishop _ _) -> True; _ -> False) pieces

kingVsKing :: Board -> [Piece] -> Bool
kingVsKing board pieces = twoPieces && bothKings
        where twoPieces = length pieces == 2
              bothKings = any (== (whiteKing board)) pieces && any (== (blackKing board)) pieces

checkmateless :: Board -> Either Outcome Board
checkmateless = ruleCheck Draw [kingVsKing, kingBishopVsKingBishop, kingKnightVsKing, kingBishopVsKing]

draw :: Board -> Either Outcome Board
--draw board = stalemate board >> fiftyMove board >> checkmateless board
draw board = stalemate board >> checkmateless board

-- Threefold repetition of a position: 
-- If it's the same player's turn to move, and there have been 3 positions throughout the game where he was in the exact same position,
-- with exactly the same possible moves => he can claim a draw (not automatic)
-- HOW
threeFold :: Board -> Either Outcome Board
threeFold = Right

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
attempt move @ (TakeEP  piece piece' piece'')     = add (perform move) . remove piece'' . remove piece
attempt move @ (TakePromote piece piece' piece'') = add (perform move) . remove piece'  . remove piece 
attempt move @ (Jump    piece piece')   = add (perform move) . remove piece
attempt move @ (Take    piece piece')   = adjustKings (perform move) . add (perform move) . remove piece
attempt move @ (Block   piece piece')   = adjustKings (perform move) . add (perform move) . remove piece
attempt move @ (Promote piece piece')   = add (perform move) . remove piece

legality :: Move -> Board -> Either Outcome Board
legality m @ (Take _ _)     board      = (turn m board) >> (available m board) >> (checks m board) >> (draw board)
legality m @ (TakeEP _ _ _) board      = (turn m board) >> (available m board) >> (checks m board) >> (draw board)
legality m @ (TakePromote _ _ _) board = (turn m board) >> (available m board) >> (checks m board) >> (draw board)
legality m @ (Block _ _)    board      = (turn m board) >> (available m board) >> (checks m board) >> (draw board)
legality m @ (Jump _ _)     board      = (turn m board) >> (available m board) >> (checks m board) >> (draw board)
legality m @ (Promote _ _)  board      = (turn m board) >> (available m board) >> (checks m board) >> (draw board)
legality m @ (CastleK _ _)  board      = (turn m board) >> (available m board) >> (checks m board) >> (draw board) >> (castles m board) 
legality m @ (CastleQ _ _)  board      = (turn m board) >> (available m board) >> (checks m board) >> (draw board) >> (castles m board)

move :: Move -> Board -> Either Outcome Board
move m = fmap (changeTurn . accumulate m . attempt m) . legality m

allMoves :: Board -> Set Move
allMoves board = foldl gather S.empty $ M.elems $ M.filter currentPlayer $ positions board
    where gather set piece = set <> (moves piece board)
          currentPlayer p  = (colour p) == (player board)

legalMoves' :: Board -> Set Move
legalMoves' board = S.filter unchecked $ allMoves board
        where unchecked m = isRight $ checks m board
           
movesFor :: (Piece -> Bool) -> Board -> Set Move
movesFor p board = S.filter unchecked $ foldl gather S.empty $ M.elems $ M.filter p $ positions board
    where gather set piece = set <> (moves piece board)
          unchecked m = isRight $ checks m board

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