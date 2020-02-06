module Chess.Internal2 where


-- What do I need to be able to do?


-- 1. Describe board geometry
-- 2. Describe and identify captures
-- 3. Describe and identity moves
-- 4. Identify types of moves and the pieces that made them
-- 5. Describe compound moves
-- 6. Identify certain special kind of moves: castling; enpassant 
-- 7. Identify trajectories of pieces'
-- 8. Block certain moves altogether


-- castling, passants and checks -> keep them at a game level, not at a piece level.

type Square = (Integer, Integer)
data Colour = B | W


data Action = Capture | Advance
data Piece = Pawn | Knight | Bishop | Rook | Queen | King
data Devel = Devel Piece Colour Action [Square] -- the piece, the action and the list of squares he visits. Last being the one where the action occurs -> more optimal: make it the head

type Move = [Devel]

data Board = Board { check  :: Bool,
                     player :: Colour,
                     king   :: (Colour, Square),
                     pieces :: [(Piece, Colour, Square)]
                     }

spread :: [a -> b] -> [a] -> [b]
spread fs as = [f a | a <- as, f <- fs] 

pawnMoves :: Colour -> Board -> Move
pawnMoves colour = spread [] . filter (pawns colour) . pieces
    where pawns W (Pawn, W, _) = True
          pawns B (Pawn, B, _) = False