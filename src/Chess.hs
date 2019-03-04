module Chess where 

import Data.Map (Map)
import qualified Data.Map as M
import Data.List (find)
import Data.Tuple (swap)
import Control.Monad (foldM, mfilter)
import Data.Maybe (maybe, catMaybes, fromMaybe)

type Pos = (Int, Int)

data Colour = B | W deriving (Eq, Show, Ord)

data Piece = 
    Pawn Int Colour   |
    Rook Int Colour   |
    Bishop Int Colour |
    Knight Int Colour |
    Queen Colour      |
    King Colour       |
    Empty
    deriving (Eq, Show, Ord)

data Move =
    Take Pos    |
    Block Pos   |
    Attack Pos  |
    Jump Pos    |
    KCastle     |
    QCastle
    deriving (Show, Eq, Ord)

data Board = Board {positions :: Map Pos Piece, pieces :: Map Piece Pos }

left :: Pos -> Pos
left (x, y) = (x - 1, y)

right :: Pos -> Pos
right (x, y) = (x + 1, y)

up :: Pos -> Pos
up (x, y) = (x, y + 1)

down :: Pos -> Pos
down (x, y) = (x, y - 1)

leftUp :: Pos -> Pos
leftUp = left . up

leftDown :: Pos -> Pos
leftDown = left . down

rightUp :: Pos -> Pos
rightUp = right . up

rightDown :: Pos -> Pos
rightDown = right . down

black :: Piece -> Bool
black (Pawn _ B) = True
black (Rook _ B) = True
black (King B) = True
black (Queen B) = True
black (Bishop _ B) = True
black (Knight _ B) = True

white :: Piece -> Bool
white = not . black

empty :: Piece -> Bool
empty Empty = True
empty _ = False

at :: Pos -> Board -> Maybe Piece
at p (Board ps _) = M.lookup p ps

when :: [(Maybe Piece -> Bool)] -> Pos -> Board -> Maybe Pos
when preds pos board = let piece = at pos board 
                       in if (any (\p -> p piece) preds) 
                          then Just pos 
                          else Nothing

maybeOpposite :: Colour -> Maybe Piece -> Bool
maybeOpposite B (Just p) = white p
maybeOpposite W (Just p) = black p
maybeOpposite _ _ = False 

maybeEmpty :: Maybe Piece -> Bool
maybeEmpty (Just p) = empty p
maybeEmpty _ = False

attack :: Pos -> Colour -> Board -> Maybe Move
attack pos colour = fmap Attack . when [maybeOpposite colour, maybeEmpty] pos

take' :: Pos -> Colour -> Board -> Maybe Move
take' pos colour = fmap Take . when [maybeOpposite colour] pos

block :: Pos -> Board -> Maybe Move
block pos = fmap Block . when [maybeEmpty] pos

jump :: Pos -> Board -> Maybe Move
jump pos = fmap Jump . when [maybeEmpty] pos

attackf :: (Pos -> Pos) -> Pos -> Colour -> Board -> [Move]
attackf f pos colour board = keep $ iterate f pos 
    where keep (pos : ps) | maybeEmpty $ at pos board = (Attack pos) : (keep ps)
          keep (pos : ps) | maybeOpposite colour $ at pos board = (Attack pos) : []
          keep  _  = []

pawnMoves :: Pos -> Colour -> Board -> [Move]
pawnMoves p colour board = catMaybes [take' (leftUp p)  colour board, 
                                      take' (rightUp p) colour board,
                                      block (up p) board,
                                      jump  (up (up p)) board]

kingMoves :: Pos -> Colour -> Board -> [Move]
kingMoves p colour board = catMaybes [attack (leftUp p) colour board, 
                                      attack (up p) colour board, 
                                      attack (rightUp p) colour board, 
                                      attack (right p) colour board,
                                      attack (rightDown p) colour board,
                                      attack (down p) colour board, 
                                      attack (leftDown p) colour board, 
                                      attack (left p) colour board,
                                      Just QCastle,
                                      Just KCastle]

rookMoves :: Pos -> Colour -> Board -> [Move]
rookMoves p colour board = (attackf up p colour board)    ++ 
                           (attackf left p colour board)  ++
                           (attackf right p colour board) ++
                           (attackf down p colour board)

bishopMoves :: Pos -> Colour -> Board -> [Move]
bishopMoves p colour board = (attackf leftUp p colour board)    ++
                             (attackf rightUp p colour board)   ++
                             (attackf leftDown p colour board)  ++
                             (attackf rightDown p colour board)

queenMoves :: Pos -> Colour -> Board -> [Move]
queenMoves p colour board = (rookMoves p colour board) ++ (bishopMoves p colour board)

knightMoves :: Pos -> Colour -> Board -> [Move]
knightMoves p colour board = catMaybes [attack (right (up (up p))) colour board,
                                        attack (right (down (down p))) colour board,
                                        attack (left (down (down p))) colour board,
                                        attack (left (up (up p))) colour board]

moves :: Pos -> Piece -> Board -> [Move]
moves pos (Pawn _ colour)   = pawnMoves pos colour
moves pos (Rook _ colour)   = rookMoves pos colour
moves pos (Bishop _ colour) = bishopMoves pos colour
moves pos (Knight _ colour) = knightMoves pos colour 
moves pos (Queen colour)    = queenMoves pos colour
moves pos (King  colour)    = kingMoves pos colour
moves pos Empty             = []

legality :: Pos -> Move -> Board -> Maybe Move
legality _ _ _ = Nothing

adapt :: Ord k => k -> (k, v) -> Map k v -> Map k v
adapt k (k', v) = M.insert k v . M.delete k

streamLine :: [(Pos, Pos)] -> Board -> Board
streamLine moves board = foldl transform board moves
    where transform (Board ps pcs) (pos, pos') = 
            let piece  = M.lookup pos  ps
                piece' = M.lookup pos' ps
                positions = maybe ps  (\piece -> adapt pos (pos', piece) ps) piece
                pieces    = maybe pcs (\piece -> M.adjust (const pos') piece pcs) piece
                pieces'   = maybe pieces (\piece -> M.delete piece pieces) piece'
            in Board { positions = positions, pieces = pieces' }

apply :: Pos -> Move -> Board -> Board
apply pos (Take pos')   = streamLine [(pos, pos')]
apply pos (Block pos')  = streamLine [(pos, pos')]
apply pos (Attack pos') = streamLine [(pos, pos')]
apply pos (Jump pos')   = streamLine [(pos, pos')]
apply pos KCastle       = streamLine [(kingPos, kingCastlePos), (rookPos, rookCastlePos)]
        where kingPos   = pos
              rookPos   = (8, 1)
              kingCastlePos = (7, 1)
              rookCastlePos = (6, 1)
              
apply pos QCastle       = streamLine [(kingPos, kingCastlePos), (rookPos, rookCastlePos)]
        where kingPos   = pos
              rookPos   = (1, 1)
              kingCastlePos = (3, 1)
              rookCastlePos = (4, 1)

move :: Pos -> Move -> Board -> Board
move pos m board = fromMaybe board newBoard
    where newBoard = do
            piece <- at pos board 
            _     <- find (== m) $ moves pos piece board
            _     <- legality pos m board
            return (apply pos m board)

---- CHESS PIECES HAVE THEIR OWN UNICODE CHARACTERS!