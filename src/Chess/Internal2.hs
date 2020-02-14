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
            Promote Action Piece   |
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

castle :: (Square -> Move) -> (Square -> Move) -> Square -> Move
castle kingf rookf square = case (kingf square, rookf square) of 
                                 (Advance k @ (King, _, _), (Advance r @ (Rook, _, _))) -> Castle k r

advanceWith :: Piece -> [(Square, Position)] -> Move
advanceWith piece ps @ (((colour, _), _) : _) = Advance (piece, colour, fmap (coord . snd) ps)

captureWith :: Piece -> [(Square, Position)] -> Move
captureWith piece ps @ (((colour, _), _) : _) = Capture (piece, colour, fmap (coord . snd) ps)

opponent :: Colour -> Position -> Bool
opponent colour (_, colour', _) = colour' /= colour

empty :: Position -> Bool
empty (piece, _, _) = piece == Empty 

--------- To check: if the coord is occupied with by enemy, if it's check and if yes, if this will escape
capturingRule :: Board -> Action -> [Position -> Bool]
capturingRule board (_, colour, _) = [const (not $ check board), opponent colour]

--------- To check: if the coord is empty, if it's check and if yes, if this will escape
advancingRule :: Board -> Action -> [Position -> Bool]
advancingRule board (_, _, _) = [const (not $ check board), empty] 

--------- To check: if the coords are empty, if any of the king coords are attacked, if there's check, if the rook hasn't moved
castlingRule :: Board -> (Action, Action) -> [[Position] -> Bool]
castlingRule board ((king, kcolour, kcoords), (rook, rcolour, rcoords)) = [const (not $ check board), all empty] -- not threatened and the rook hasn't moved

--------- To check: if it's the last rank, if it's taking something, if there's check and if yes, if this will escape
promotingRule :: Board -> (Action, Piece) -> [Position -> Bool]
promotingRule board ((Pawn, W, _), piece) = [const (not $ check board), (== 8) . y . coord, empty] -- or opponent and was in position to take
promotingRule board ((Pawn, B, _), piece) = [const (not $ check board), (== 1) . y . coord, empty] -- or opponent and was in position to take
promotingRule board (_, _)                = []

--------- To check: If the coord I'm going towards is empty, if the last move was a two coord advance, if there's check and if yes, if this will escape
enpassantRule :: Board -> Action -> [Position -> Bool]
enpassantRule board (Pawn, _, _) = [const (not $ check board), empty] -- the last move is an opponents pawn doing a two coord advance

verify :: Board -> Move -> Maybe Move
verify board move = Nothing --if (allowed move) then Just move else Nothing
    --where mergeCoords ((k, kc, ks), (r, rc, rs)) = ks <> rs
          --allowed (Capture action)   = isJust $ mfilter (every (capturingRule board action))   $ lookAt board $ head $ coords action
          --allowed (Advance action)   = isJust $ mfilter (every (advancingRule board action))   $ lookAt board $ head $ coords action
          --allowed (Promote position) = isJust $ mfilter (every (promotingRule board position)) $ lookAt board $ head $ coords $ fst position
          --allowed (Enpassant action) = isJust $ mfilter (every (enpassantRule board action))   $ lookAt board $ head $ coords action
          --allowed (Castle actions)   = isJust $ mfilter (every (castlingRule board actions))   $ traverse (lookAt board) $ mergeCoords actions

pawnMoves :: Board -> Square -> [Move]
pawnMoves board = spreadM [verify board . captureWith Pawn . takeWhile opponent' . take 1 . follow board UL,
                           verify board . captureWith Pawn . takeWhile opponent' . take 1 . follow board UR,
                           verify board . advanceWith Pawn . takeWhile empty'    . take 1 . follow board U,
                           verify board . advanceWith Pawn . takeWhile empty'    . take 2 . follow board U]


bishopMoves :: Board -> Square -> [Move]
bishopMoves board = spreadM [verify board . captureWith Bishop . keepUntil opponent' empty' . follow board UR,
                             verify board . captureWith Bishop . keepUntil opponent' empty' . follow board UL,
                             verify board . captureWith Bishop . keepUntil opponent' empty' . follow board DR,
                             verify board . captureWith Bishop . keepUntil opponent' empty' . follow board DL]

rookMoves :: Board -> Square -> [Move]
rookMoves board = spreadM [verify board . captureWith Rook . keepUntil opponent' empty' . follow board U,
                           verify board . captureWith Rook . keepUntil opponent' empty' . follow board U,
                           verify board . advanceWith Rook . takeWhile empty' . follow board U,
                           verify board . advanceWith Rook . takeWhile empty' . follow board U]

queenMoves :: Board -> Square -> [Move]
queenMoves board = spreadM [verify board . captureWith Queen . keepUntil opponent' empty' . follow board UR,
                            verify board . captureWith Queen . keepUntil opponent' empty' . follow board UL,
                            verify board . captureWith Queen . keepUntil opponent' empty' . follow board DR,
                            verify board . captureWith Queen . keepUntil opponent' empty' . follow board DL] 

kingMoves :: Board -> Square -> [Move]
kingMoves board = spreadM [verify board . captureWith King . takeWhile opponent' . take 1 . follow board U,
                           verify board . captureWith King . takeWhile opponent' . take 1 . follow board D,
                           verify board . captureWith King . takeWhile opponent' . take 1 . follow board L,
                           verify board . captureWith King . takeWhile opponent' . take 1 . follow board R,
                           verify board . captureWith King . takeWhile opponent' . take 1 . follow board UL,
                           verify board . captureWith King . takeWhile opponent' . take 1 . follow board UR,
                           verify board . captureWith King . takeWhile opponent' . take 1 . follow board DL,
                           verify board . captureWith King . takeWhile opponent' . take 1 . follow board DR,

                           verify board . advanceWith King . takeWhile empty' . take 1 . follow board U,
                           verify board . advanceWith King . takeWhile empty' . take 1 . follow board D,
                           verify board . advanceWith King . takeWhile empty' . take 1 . follow board L,
                           verify board . advanceWith King . takeWhile empty' . take 1 . follow board R,
                           verify board . advanceWith King . takeWhile empty' . take 1 . follow board UL,
                           verify board . advanceWith King . takeWhile empty' . take 1 . follow board UR,
                           verify board . advanceWith King . takeWhile empty' . take 1 . follow board DL,
                           verify board . advanceWith King . takeWhile empty' . take 1 . follow board DR,
                           
                           verify board . castle (advanceWith King . takeWhile empty' . take 2 . follow board R)
                                                 (advanceWith Rook . takeWhile empty' . take 2 . follow board L . kingside),
                           verify board . castle (advanceWith King . takeWhile empty' . take 2 . follow board L)
                                                 (advanceWith Rook . takeWhile empty' . take 3 . follow board R . queenside)]
        where kingside  (W, _) = (W, (8, 1))
              kingside  (B, _) = (B, (8, 8))
              queenside (W, _) = (W, (1, 1))
              queenside (B, _) = (B, (1, 8))

empty' :: (Square, Position) -> Bool
empty' (_, (p, _, _)) = p == Empty

opponent' :: (Square, Position) -> Bool
opponent' ((c, _), (_, c', _)) = c /= c'

oneOf :: [(a -> Bool)] -> a -> Bool
oneOf fs a = isJust $ find (\f -> f a) fs