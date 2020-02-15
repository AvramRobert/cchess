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
                     actions :: [Action],
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

y :: Coord -> Int
y = snd

x :: Coord -> Int
x = fst

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

follow :: Board -> Dir -> Square -> [(Square, Position)]
follow board dir square     = proceed $ lookAhead square
    where proceed (Just p)  = toList $ unfoldr step p
          proceed (Nothing) = []
          step position'    = ((square, position'), lookAhead (colour position', coord position'))
          lookAhead         = lookAt board . snd . develop dir


colour :: Position -> Colour
colour (_, c, _) = c

coord :: Position -> Coord
coord (_, _, s) = s

coords :: Action -> [Coord]
coords (_, _, sqs) = sqs

lookAt :: Board -> Coord -> Maybe Position
lookAt board coord' = find ((== coord') . coord) $ pieces board

castle :: Board -> (Square -> Maybe Move) -> (Square -> Maybe Move) -> Square -> Maybe Move
castle board kingf rookf square = do
            kingMove   <- kingf square
            rookMove   <- rookf square
            let castle = createFrom kingMove rookMove
            permitted board $ castle
      where createFrom (Advance k) (Advance r) = Castle k r

advanceWith :: Piece -> Board -> [(Square, Position)] -> Maybe Move
advnaceWith piece board []                          = Nothing
advanceWith piece board ps @ (((colour, _), _) : _) = permitted board $ Advance (piece, colour, fmap (coord . snd) ps)

captureWith :: Piece -> Board -> [(Square, Position)] -> Maybe Move
captureWith piece board []                          = Nothing
captureWith piece board ps @ (((colour, _), _) : _) = permitted board $ Capture (piece, colour, fmap (coord . snd) ps)

empty :: (Square, Position) -> Bool
empty (_, (p, _, _)) = p == Empty

opponent :: (Square, Position) -> Bool
opponent ((c, _), (_, c', _)) = c /= c'


-- This should now check the current global chess rules: check, check escape or mate
--- NOTE: This checks the influence of the CURRENT chess rules on the move. The move itself, after being performed, has its own influence on the game
-- Therefor, check / mate has to be checked twice. Once before and once after. 
permitted :: Board -> Move -> Maybe Move
permitted board = Just



-- captures, advances and such should return maybes and probably check general chess rules. Like check and check escape.
-- the individual moves themselves should make sure however that their own constraints are checked locally
-- for example: enpassant => enapssant Pawn . takeWhile (every [opponent . nextTo, jump board])
pawnMoves :: Board -> Square -> [Move]
pawnMoves board = spreadM [captureWith Pawn board . takeWhile opponent . take 1 . follow board UL,
                           captureWith Pawn board . takeWhile opponent . take 1 . follow board UR,
                           advanceWith Pawn board . takeWhile empty    . take 1 . follow board U,
                           advanceWith Pawn board . takeWhile empty    . take 2 . follow board U]


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

queenMoves :: Board -> Square -> [Move]
queenMoves board = spreadM [captureWith Queen board . keepUntil opponent empty . follow board UR,
                            captureWith Queen board . keepUntil opponent empty . follow board UL,
                            captureWith Queen board . keepUntil opponent empty . follow board DR,
                            captureWith Queen board . keepUntil opponent empty . follow board DL] 

kingMoves :: Board -> Square -> [Move]
kingMoves board = spreadM [captureWith King board . takeWhile opponent . take 1 . follow board U,
                           captureWith King board . takeWhile opponent . take 1 . follow board D,
                           captureWith King board . takeWhile opponent . take 1 . follow board L,
                           captureWith King board . takeWhile opponent . take 1 . follow board R,
                           captureWith King board . takeWhile opponent . take 1 . follow board UL,
                           captureWith King board . takeWhile opponent . take 1 . follow board UR,
                           captureWith King board . takeWhile opponent . take 1 . follow board DL,
                           captureWith King board . takeWhile opponent . take 1 . follow board DR,

                           advanceWith King board . takeWhile empty . take 1 . follow board U,
                           advanceWith King board . takeWhile empty . take 1 . follow board D,
                           advanceWith King board . takeWhile empty . take 1 . follow board L,
                           advanceWith King board . takeWhile empty . take 1 . follow board R,
                           advanceWith King board . takeWhile empty . take 1 . follow board UL,
                           advanceWith King board . takeWhile empty . take 1 . follow board UR,
                           advanceWith King board . takeWhile empty . take 1 . follow board DL,
                           advanceWith King board . takeWhile empty . take 1 . follow board DR,
                           
                           castle board (advanceWith King board . takeWhile empty . take 2 . follow board R)
                                        (advanceWith Rook board . takeWhile empty . take 2 . follow board L . kingside),
                           castle board (advanceWith King board . takeWhile empty . take 2 . follow board L)
                                        (advanceWith Rook board . takeWhile empty . take 3 . follow board R . queenside)]
        where kingside  (W, _) = (W, (8, 1))
              kingside  (B, _) = (B, (8, 8))
              queenside (W, _) = (W, (1, 1))
              queenside (B, _) = (B, (1, 8))