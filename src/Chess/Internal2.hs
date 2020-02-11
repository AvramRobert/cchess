module Chess.Internal2 where

import Data.List (find)
import Control.Monad (mfilter)
import Data.Functor ((<$))
import Data.Maybe (maybe)

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

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)
data Action = Action Piece Colour [Square] deriving (Eq, Show) -- squares should always be in reverse order of action => head is the last square

data Move = Capture Action   |
            Advance Action   |
            Promote Action   |
            Castle [Action]   -- this is the only thing that is sort-of compound
            deriving (Eq, Show) 

data Dir = U  | D  | L  | R |
           UL | UR | DL | DR deriving (Show, Eq)

data Board = Board { check  :: Bool,
                     player :: Colour,
                     pieces :: [(Piece, Colour, Square)]
                     } deriving (Eq, Show)

spread :: [a -> b] -> a -> [b]
spread fs a = [f a | f <- fs] 

develop :: [Dir] -> (Piece, Colour, Square) -> (Piece, Colour, [Square]) 
develop dir (piece, colour, square) = (piece, colour, scanl (develop' colour) square dir)
    where develop' W (x, y) L  = (x - 1, y)
          develop' W (x, y) R  = (x + 1, y)
          develop' W (x, y) U  = (x, y + 1)
          develop' W (x, y) D  = (x, y - 1)
          develop' W (x, y) UL = (x - 1, y + 1)
          develop' W (x, y) UR = (x + 1, y + 1)
          develop' W (x, y) DL = (x - 1, y - 1)
          develop' W (x, y) DR = (x + 1, y - 1)
          develop' B (x, y) L  = (x - 1, y)
          develop' B (x, y) R  = (x + 1, y)
          develop' B (x, y) U  = (x, y - 1)
          develop' B (x, y) D  = (x, y + 1)
          develop' B (x, y) UL = (x - 1, y - 1)
          develop' B (x, y) UR = (x + 1, y - 1)
          develop' B (x, y) DL = (x - 1, y + 1)
          develop' B (x, y) DR = (x + 1, y + 1)

colour :: (Piece, Colour, Square) -> Colour
colour (_, c, _) = c

lookAt :: Square -> Board -> Maybe (Piece, Colour, Square)
lookAt square = find position . pieces
    where position (_, _, square') = square == square'

advance :: [Dir] -> (Piece, Colour, Square) -> Move
advance dir = create . develop dir
    where create (piece, colour, squares) = Advance (Action piece colour squares)

capture :: [Dir] -> (Piece, Colour, Square) -> Move
capture dir = create . develop dir
    where create (piece, colour, squares) = Capture (Action piece colour squares)

verify :: Move -> Board -> Maybe Move
verify m @ (Capture (Action piece colour' squares))                                   = Just m <$ mfilter ((/= colour') . colour) . lookAt (head squares) -- this might actually work
verify m @ (Advance (Action piece colour' squares))                                   = maybe (Just m) (const Nothing) . lookAt (head squares)
verify m @ (Castle  [(Action King kcolour ksquares), (Action Rook rcolour rsquares)]) = const Nothing

-- in order to keep it rather abstract at the usage level
-- I have to check the validity of a move a the point where it is defined, not in a separate function
-- If I do this, then only the local details of every move definition will contain specialised code, the usage functions and everything else will be kept generalised
pawnMoves :: (Piece, Colour, Square) -> [Move]
pawnMoves = spread [capture [UL],
                    capture [UR],
                    advance [U],
                    advance [U, U]]