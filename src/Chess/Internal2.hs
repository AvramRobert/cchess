module Chess.Internal2 where

import Data.List (find)
import Data.List.NonEmpty (unfoldr, toList)
import Control.Monad (mfilter, join)
import Data.Functor (($>))
import Data.Maybe (maybe, isJust, isNothing, catMaybes, fromJust)

-- What do I need to be able to do?

-- TODO: Re-add the pgn files for testing

-- 1. Describe board geometry
-- 2. Describe and identify captures
-- 3. Describe and identity moves
-- 4. Identify types of moves and the pieces that made them
-- 5. Describe compound moves
-- 6. Identify certain special kind of moves: castling; enpassant 
-- 7. Identify trajectories of pieces'
-- 8. Block certain moves altogether
-- 9. Do promotions

-- castling, passants and checks -> keep them at a game level, not at a piece level.

type Coord    = (Int, Int)
data Colour   = B | W deriving (Eq, Show)
data Piece    = Pawn | Knight | Bishop | Rook | Queen | King | Empty deriving (Eq, Show)
type Square   = (Colour, Coord)
type Position = (Piece, Colour, Coord) 
type Action   = (Piece, Colour, [Coord]) -- coords should always be in reverse order of action => head is the last coord

data Move = Capture Action          |
            Advance Action          |
            Enpassant Action        |
            Promote Action Piece    |
            Castle Action Action
            deriving (Eq, Show) 

data Dir = U  | D  | L  | R |
           UL | UR | DL | DR deriving (Show, Eq)

data Board = Board { check   :: Bool,
                     player  :: Colour,
                     past    :: [Move],
                     pieces  :: [(Piece, Colour, Coord)]
                     } deriving (Eq, Show)

spread :: [a -> b] -> a -> [b]
spread fs a = [f a | f <- fs]

spreadM :: [a -> Maybe b] -> a -> [b]
spreadM [] a       = []
spreadM (f : fs) a = case (f a) of (Just b)  -> b : (spreadM fs a)
                                   (Nothing) -> spreadM fs a

every :: [a -> Bool] -> a -> Bool
every (p : ps) a = p a && every ps a

oneOf :: [(a -> Bool)] -> a -> Bool
oneOf [] a = True
oneOf fs a = isJust $ find (\f -> f a) fs

keepUntil :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
keepUntil stop keep (a : as) | keep a && stop a = a : []
keepUntil stop keep (a : as) | keep a = a : (keepUntil stop keep as)
keepUntil stop keep (a : as) | stop a = a : []
keepUntil _ _ _                       = []

develop :: Dir -> Square -> Square
develop dir (colour, (x, y)) = (colour, towards dir colour)
    where towards L  W = (x - 1, y)
          towards R  W = (x + 1, y)
          towards U  W = (x, y + 1)
          towards D  W = (x, y - 1)
          towards UL W = (x - 1, y + 1)
          towards UR W = (x + 1, y + 1)
          towards DL W = (x - 1, y - 1)
          towards DR W = (x + 1, y - 1)
          towards L  B = (x - 1, y)
          towards R  B = (x + 1, y)
          towards U  B = (x, y - 1)
          towards D  B = (x, y + 1)
          towards UL B = (x - 1, y - 1)
          towards UR B = (x + 1, y - 1)
          towards DL B = (x - 1, y + 1)
          towards DR B = (x + 1, y + 1)

lookAhead :: Board -> Dir -> Square -> Maybe Position
lookAhead board dir = lookAt board . snd . develop dir

-- follow :: Board -> Dir -> Square -> [(Square, Position)]
-- follow board dir square     = proceed $ lookAhead square
--     where proceed (Just p)  = toList $ unfoldr step p
--           proceed (Nothing) = []
--           step position'    = ((square, position'), lookAhead (colour position', coord position'))
--           lookAhead         = lookAt board . snd . develop dir

follow :: Board -> Dir -> Square -> [(Square, Position)]
follow board dir square = go [] $ lookAhead board dir square
      where go xs (Just position) = go ((square, position) : xs) (lookAhead board dir $ nextSquare position)
            go xs (Nothing)       = xs
            nextSquare position   = (colour position, coord position)

follow' :: Board -> [Dir] -> Square -> [(Square, Position)]
follow' board all @ (d : ds) square       = go [] ds (lookAhead board d square) 
      where go xs (d : ds) (Just position)       = go ((square, position) : xs) ds (lookAhead board d $ nextSquare position)
            go xs [] _ | length all == length xs = xs
            go xs _ _                            = []
            nextSquare position                  = (colour position, coord position)

piece :: Position -> Piece
piece (p, _, _) = p

colour :: Position -> Colour
colour (_, c, _) = c

coord :: Position -> Coord
coord (_, _, s) = s

square :: Position -> Square
square (_, c, s) = (c, s)

lookAt :: Board -> Coord -> Maybe Position
lookAt board coord' = find ((== coord') . coord) $ pieces board

trace :: Piece -> [(Square, Position)] -> Action
trace piece path @ (((colour, _), _) : _) = (piece, colour, construct path)
      where construct []            = []
            construct ((s, p) : []) = coord p : (snd s) : []
            construct ((s, p) : ps) = coord p : (construct ps)

castle :: Board -> (Square -> Maybe Move) -> (Square -> Maybe Move) -> Square -> Maybe Move
castle board kingf rookf square = do
            kingMove   <- kingf square
            rookMove   <- rookf square
            let castle = createFrom kingMove rookMove
            permitted board $ castle
      where createFrom (Advance k) (Advance r) = Castle k r

advanceWith :: Piece -> Board -> [(Square, Position)] -> Maybe Move
advnaceWith piece board []   = Nothing
advanceWith piece board path = permitted board $ Advance $ trace piece path 

captureWith :: Piece -> Board -> [(Square, Position)] -> Maybe Move
captureWith piece board []   = Nothing
captureWith piece board path = permitted board $ Capture $ trace piece path

enpassant :: Board -> [(Square, Position)] -> Maybe Move
enpassant board []   = Nothing
enpassant board path = permitted board $ Enpassant $ trace Pawn path

empty :: (Square, Position) -> Bool
empty (_, (p, _, _)) = p == Empty

opponent :: (Square, Position) -> Bool
opponent ((c, _), (_, c', _)) = c /= c'

jumped :: Board -> (Square, Position) -> Bool
jumped board ((colour, (x, y)), _)  = isJust $ mfilter (every [leaped, opposing]) $ keepFirst $ past board 
      where keepFirst ((Advance a) : _) = Just a
            keepFirst []                = Nothing
            opposing (piece, c, as)     = (piece == Pawn) && (colour /= c) && sameFile as
            sameFile ((x', _) : _)      = x == x' 
            leaped   (_, W, as)         = (== [4, 3, 2]) $ fmap snd as
            leaped   (_, B, as)         = (== [7, 6, 5]) $ fmap snd as

started :: Board -> (Square, Position) -> Bool
started board ((colour, coord), _)     = isJust $ mfilter (pawnOf colour) $ lookAt board coord
      where pawnOf W (Pawn, W, (_, 2)) = True
            pawnOf B (Pawn, B, (_, 7)) = True
            pawnOf _ _                 = False

threats :: Board -> [Square] -> [Move]
threats board sqs = filter (oneOf (fmap attacks sqs)) $ pieces board >>= (movesFor board)
      where attacks (c, s) (Capture   (_, c', s' : _)) = c /= c' && s == s'
            attacks (c, s) (Enpassant (_, c', s' : _)) = c /= c' && s == s' -- i don't think this is right
            attacks _ _                                = False  

king :: Board -> Colour -> Position
king board colour' = fromJust $ find (every [(== colour') . colour, (== King) . piece]) $ pieces board

-- There should be a way to just simulate this without actually creating a new board every time
-- In addition, the global threats of a board are constant over a complete pass of move compilation.
-- That means that I could theoretically compute them once and hand them individually to every `*moves` function
permitted :: Board -> Move -> Maybe Move
permitted board move | not $ check board                = Just move
permitted board move | not $ check $ perform board move = Just move
permitted board _                                       = Nothing 

pawnMoves :: Board -> Square -> [Move]
pawnMoves board = spreadM [captureWith Pawn board . takeWhile opponent                       . follow' board [UL],
                           captureWith Pawn board . takeWhile opponent                       . follow' board [UR],
                           advanceWith Pawn board . takeWhile empty                          . follow' board [U],
                           advanceWith Pawn board . takeWhile (every [started board, empty]) . follow' board [U, U],
                           enpassant board        . takeWhile (every [jumped board, empty])  . follow' board [UL],
                           enpassant board        . takeWhile (every [jumped board, empty])  . follow' board [UR]]


bishopMoves :: Board -> Square -> [Move]
bishopMoves board = spreadM [captureWith Bishop board . keepUntil opponent empty . follow board UR,
                             captureWith Bishop board . keepUntil opponent empty . follow board UL,
                             captureWith Bishop board . keepUntil opponent empty . follow board DR,
                             captureWith Bishop board . keepUntil opponent empty . follow board DL]

rookMoves :: Board -> Square -> [Move]
rookMoves board = spreadM [captureWith Rook board . keepUntil opponent empty . follow board U,
                           captureWith Rook board . keepUntil opponent empty . follow board U,
                           advanceWith Rook board . takeWhile empty . follow board U,
                           advanceWith Rook board . takeWhile empty . follow board U]

knightMoves :: Board -> Square -> [Move]
knightMoves board = spreadM [captureWith Knight board . keepUntil opponent empty . follow' board [U, U, L],
                             captureWith Knight board . keepUntil opponent empty . follow' board [U, U, R],
                             captureWith Knight board . keepUntil opponent empty . follow' board [D, D, L],
                             captureWith Knight board . keepUntil opponent empty . follow' board [D, D, R],
                             captureWith Knight board . keepUntil opponent empty . follow' board [L, L, U],
                             captureWith Knight board . keepUntil opponent empty . follow' board [L, L, D],
                             captureWith Knight board . keepUntil opponent empty . follow' board [R, R, U],
                             captureWith Knight board . keepUntil opponent empty . follow' board [R, R, D],

                             advanceWith Knight board . takeWhile empty . follow' board [U, U, L],
                             advanceWith Knight board . takeWhile empty . follow' board [U, U, R],
                             advanceWith Knight board . takeWhile empty . follow' board [D, D, L],
                             advanceWith Knight board . takeWhile empty . follow' board [D, D, R],
                             advanceWith Knight board . takeWhile empty . follow' board [L, L, U],
                             advanceWith Knight board . takeWhile empty . follow' board [L, L, D],
                             advanceWith Knight board . takeWhile empty . follow' board [R, R, U],
                             advanceWith Knight board . takeWhile empty . follow' board [R, R, D]]
                             

queenMoves :: Board -> Square -> [Move]
queenMoves board = spreadM [captureWith Queen board . keepUntil opponent empty . follow board UR,
                            captureWith Queen board . keepUntil opponent empty . follow board UL,
                            captureWith Queen board . keepUntil opponent empty . follow board DR,
                            captureWith Queen board . keepUntil opponent empty . follow board DL,
                            captureWith Queen board . keepUntil opponent empty . follow board U,
                            captureWith Queen board . keepUntil opponent empty . follow board D,
                            captureWith Queen board . keepUntil opponent empty . follow board L,
                            captureWith Queen board . keepUntil opponent empty . follow board R,
                            
                            advanceWith Queen board . takeWhile empty . follow board UR,
                            advanceWith Queen board . takeWhile empty . follow board UL,
                            advanceWith Queen board . takeWhile empty . follow board DR,
                            advanceWith Queen board . takeWhile empty . follow board DL,
                            advanceWith Queen board . takeWhile empty . follow board U,
                            advanceWith Queen board . takeWhile empty . follow board D,
                            advanceWith Queen board . takeWhile empty . follow board L,
                            advanceWith Queen board . takeWhile empty . follow board R] 

kingMoves :: Board -> Square -> [Move]
kingMoves board = spreadM [captureWith King board . takeWhile opponent . follow' board [U],
                           captureWith King board . takeWhile opponent . follow' board [D],
                           captureWith King board . takeWhile opponent . follow' board [L],
                           captureWith King board . takeWhile opponent . follow' board [R],
                           captureWith King board . takeWhile opponent . follow' board [UL],
                           captureWith King board . takeWhile opponent . follow' board [UR],
                           captureWith King board . takeWhile opponent . follow' board [DL],
                           captureWith King board . takeWhile opponent . follow' board [DR],

                           advanceWith King board . takeWhile empty . follow' board [U],
                           advanceWith King board . takeWhile empty . follow' board [D],
                           advanceWith King board . takeWhile empty . follow' board [L],
                           advanceWith King board . takeWhile empty . follow' board [R],
                           advanceWith King board . takeWhile empty . follow' board [UL],
                           advanceWith King board . takeWhile empty . follow' board [UR],
                           advanceWith King board . takeWhile empty . follow' board [DL],
                           advanceWith King board . takeWhile empty . follow' board [DR],
                           
                           castle board (advanceWith King board . takeWhile empty . follow' board [R, R])
                                        (advanceWith Rook board . takeWhile empty . follow' board [L, L] . kingside),
                           castle board (advanceWith King board . takeWhile empty . follow' board [L, L])
                                        (advanceWith Rook board . takeWhile empty . follow' board [R, R, R] . queenside)]
        where kingside  (W, _) = (W, (8, 1))
              kingside  (B, _) = (B, (8, 8))
              queenside (W, _) = (W, (1, 1))
              queenside (B, _) = (B, (1, 8))

moves :: Board -> Square -> [Move]
moves board = join . spread [pawnMoves board, kingMoves board, rookMoves board, bishopMoves board, knightMoves board, queenMoves board]

movesFor :: Board -> Position -> [Move]
movesFor board (Pawn, c, s)   = pawnMoves board (c, s)
movesFor board (King, c, s)   = kingMoves board (c, s)
movesFor board (Rook, c, s)   = rookMoves board (c, s)
movesFor board (Bishop, c, s) = bishopMoves board (c, s)
movesFor board (Queen, c, s)  = queenMoves board (c, s)
movesFor board (Knight, c, s) = knightMoves board (c, s)

-- this can be optimised, If i've already done one or the other, i can stop once i do the second
reconstruct :: Position -> Position-> [Position] -> [Position]
reconstruct old new []                              = []
reconstruct old new (a : as) | coord a == coord old = (Empty, colour old, coord a) : (reconstruct old new as)
reconstruct old new (a : as) | coord a == coord new = new : (reconstruct old new as)

perform :: Board -> Move -> Board
perform board move = let board' = board { pieces = commit move $ pieces board, 
                                          past   = move : (past board) }
                         kings  = [square $ king board' W, square $ king board' B]
                     in  board' { check = not $ null $ threats board' kings }
            where start (p, c, cs)                = (p, c, last cs)
                  end   (p, c, cs)                = (p, c, head cs)
                  commit (Capture action)         = reconstruct (start action) (end action)
                  commit (Advance action)         = reconstruct (start action) (end action)
                  commit (Enpassant action)       = const [] -- this is a tad bit different
                  commit (Castle kaction raction) = reconstruct (start raction) (end raction) . reconstruct (start kaction) (end kaction) 