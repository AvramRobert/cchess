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

type Square = (Int, Int)

data Colour = B | W deriving (Eq, Show)

data Piece = Pawn | Knight | Bishop | Rook | Queen | King | Empty deriving (Eq, Show)

type Position = (Piece, Colour, Square)
type Action   = (Piece, Colour, [Square]) -- squares should always be in reverse order of action => head is the last square

data Move = Capture Action            |
            Advance Action            |
            Enpassant Action          |
            Promote (Action, Piece)   |
            Castle  (Action, Action)
            deriving (Eq, Show) 

data Dir = U  | D  | L  | R |
           UL | UR | DL | DR deriving (Show, Eq)

data Board = Board { check   :: Bool,
                     player  :: Colour,
                     actions :: [Action],
                     pieces  :: [(Piece, Colour, Square)]
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

y :: Square -> Int
y = snd

x :: Square -> Int
x = fst

develop :: Dir -> Position -> Position
develop dir (piece, colour, (x, y)) = (piece, colour, towards dir colour)
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

follow :: Board -> Dir -> Position -> [(Position, Position)]
follow board dir position = proceed $ lookAhead position
    where proceed (Just p)    = toList $ unfoldr step p
          proceed (Nothing)   = []
          step position'      = ((position, position'), lookAhead position')
          lookAhead position' = lookAt board $ square $ develop dir position'

colour :: Position -> Colour
colour (_, c, _) = c

square :: Position -> Square
square (_, _, s) = s

squares :: Action -> [Square]
squares (_, _, sqs) = sqs

lookAt :: Board -> Square -> Maybe Position
lookAt board square' = find ((== square') . square) $ pieces board

advance :: [(Position, Position)] -> Move
advance (((p, c, s), _) : ps) = Advance (p, c, s : (fmap (square . fst) ps))

capture :: [(Position, Position)]  -> Move
capture (((p, c, s), _) : ps) = Capture (p, c, s : (fmap (square . fst) ps))

opponent :: Colour -> Position -> Bool
opponent colour (_, colour', _) = colour' /= colour

empty :: Position -> Bool
empty (piece, _, _) = piece == Empty 

--------- To check: if the square is occupied with by enemy, if it's check and if yes, if this will escape
capturingRule :: Board -> Action -> [Position -> Bool]
capturingRule board (_, colour, _) = [const (not $ check board), opponent colour]

--------- To check: if the square is empty, if it's check and if yes, if this will escape
advancingRule :: Board -> Action -> [Position -> Bool]
advancingRule board (_, _, _) = [const (not $ check board), empty] 

--------- To check: if the squares are empty, if any of the king squares are attacked, if there's check, if the rook hasn't moved
castlingRule :: Board -> (Action, Action) -> [[Position] -> Bool]
castlingRule board ((king, kcolour, ksquares), (rook, rcolour, rsquares)) = [const (not $ check board), all empty] -- not threatened and the rook hasn't moved

--------- To check: if it's the last rank, if it's taking something, if there's check and if yes, if this will escape
promotingRule :: Board -> (Action, Piece) -> [Position -> Bool]
promotingRule board ((Pawn, W, _), piece) = [const (not $ check board), (== 8) . y . square, empty] -- or opponent and was in position to take
promotingRule board ((Pawn, B, _), piece) = [const (not $ check board), (== 1) . y . square, empty] -- or opponent and was in position to take
promotingRule board (_, _)                = []

--------- To check: If the square I'm going towards is empty, if the last move was a two square advance, if there's check and if yes, if this will escape
enpassantRule :: Board -> Action -> [Position -> Bool]
enpassantRule board (Pawn, _, _) = [const (not $ check board), empty] -- the last move is an opponents pawn doing a two square advance

verify :: Board -> Move -> Maybe Move
verify board move = if (allowed move) then Just move else Nothing
    where mergeSquares ((k, kc, ks), (r, rc, rs)) = ks <> rs
          allowed (Capture action)   = isJust $ mfilter (every (capturingRule board action))   $ lookAt board $ head $ squares action
          allowed (Advance action)   = isJust $ mfilter (every (advancingRule board action))   $ lookAt board $ head $ squares action
          allowed (Promote position) = isJust $ mfilter (every (promotingRule board position)) $ lookAt board $ head $ squares $ fst position
          allowed (Enpassant action) = isJust $ mfilter (every (enpassantRule board action))   $ lookAt board $ head $ squares action
          allowed (Castle actions)   = isJust $ mfilter (every (castlingRule board actions))   $ traverse (lookAt board) $ mergeSquares actions

pawnMoves :: Board -> Position -> [Move]
pawnMoves board = spreadM [verify board . capture . takeWhile opponent' . take 1 . follow board UL,
                           verify board . capture . takeWhile opponent' . take 1 . follow board UR,
                           verify board . advance . takeWhile empty'    . take 1 . follow board U,
                           verify board . advance . takeWhile empty'    . take 2 . follow board U]


bishopMoves :: Board -> Position -> [Move]
bishopMoves board = spreadM [verify board . capture . keepUntil opponent' empty' . follow board UR,
                             verify board . capture . keepUntil opponent' empty' . follow board UL,
                             verify board . capture . keepUntil opponent' empty' . follow board DR,
                             verify board . capture . keepUntil opponent' empty' . follow board DL]

rookMoves :: Board -> Position -> [Move]
rookMoves board = spreadM [verify board . capture . keepUntil opponent' empty' . follow board U,
                           verify board . capture . keepUntil opponent' empty' . follow board U,
                           verify board . advance . takeWhile empty' . follow board U,
                           verify board . advance . takeWhile empty' . follow board U]

queenMoves :: Board -> Position -> [Move]
queenMoves board = join . spread [bishopMoves board, rookMoves board]

kingMoves :: Board -> Position -> [Move]
kingMoves board = spreadM [verify board . capture . takeWhile opponent' . take 1 . follow board U,
                           verify board . capture . takeWhile opponent' . take 1 . follow board D,
                           verify board . capture . takeWhile opponent' . take 1 . follow board L,
                           verify board . capture . takeWhile opponent' . take 1 . follow board R,
                           verify board . capture . takeWhile opponent' . take 1 . follow board UL,
                           verify board . capture . takeWhile opponent' . take 1 . follow board UR,
                           verify board . capture . takeWhile opponent' . take 1 . follow board DL,
                           verify board . capture . takeWhile opponent' . take 1 . follow board DR,

                           verify board . advance . takeWhile empty' . take 1 . follow board U,
                           verify board . advance . takeWhile empty' . take 1 . follow board D,
                           verify board . advance . takeWhile empty' . take 1 . follow board L,
                           verify board . advance . takeWhile empty' . take 1 . follow board R,
                           verify board . advance . takeWhile empty' . take 1 . follow board UL,
                           verify board . advance . takeWhile empty' . take 1 . follow board UR,
                           verify board . advance . takeWhile empty' . take 1 . follow board DL,
                           verify board . advance . takeWhile empty' . take 1 . follow board DR,
                           
                           verify board . castle . tupled (takeWhile empty' . take 2 . follow board R, 
                                                           takeWhile empty' . take 2 . follow board L),
                           verify board . castle . tupled (takeWhile empty' . take 2 . follow board L,
                                                           takeWhile empty' . take 3 . follow board R)]

tupled :: (a -> b) -> (a -> c) -> a -> (a, c)
tupled f g a = (f a, g a)

empty' :: (Position, Position) -> Bool
empty' (_, (p, _, _)) = p == Empty

opponent' :: (Position, Position) -> Bool
opponent' ((_, c, _), (_, c', _)) = c /= c'

oneOf :: [(a -> Bool)] -> a -> Bool
oneOf fs a = isJust $ find (\f -> f a) fs