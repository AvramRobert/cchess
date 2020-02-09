module Chess.Internal2 where


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


data Action = Capture | Advance deriving (Eq, Show)
data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Show)
data Development = Development Action Piece Colour [Square] deriving (Eq, Show)
-- the piece, the action and the list of squares he visits. Last being the one where the action occurs -> more optimal: make it the head

data Move = Single Development |
            Compound [Development]

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

advance :: [Dir] -> (Piece, Colour, Square) -> Development
advance dir = create . develop dir
    where create (piece, colour, squares) = Development Advance piece colour squares

capture :: [Dir] -> (Piece, Colour, Square) -> Development
capture dir = create . develop dir
    where create (piece, colour, squares) = Development Capture piece colour squares

verify :: Action -> Board -> Move -> Maybe Move
verify Capture board (Single (Development Capture piece colour squares)) = Nothing -- this might actually work

-- in order to keep it rather abstract at the usage level
-- I have to check the validity of a move a the point where it is defined, not in a separate function
-- If I do this, then only the local details of every move definition will contain specialised code, the usage functions and everything else will be kept generalised
pawnMoves :: (Piece, Colour, Square) -> [Move]
pawnMoves = spread [Single . capture [UL],
                    Single . capture [UR],
                    Single . advance [U],
                    Single . advance [U, U]]