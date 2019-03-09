module Chess where 

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (find)
import Data.Tuple (swap)
import Control.Monad (foldM, mfilter)
import Control.Applicative ((<|>))
import Data.Maybe (maybe, catMaybes, fromMaybe, isNothing)

type Pos = (Int, Int)

data Colour = B | W | T deriving (Eq, Show, Ord)

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

data Board = Board {positions :: Map Pos Piece, pieces :: Map Piece Pos}

(>?=) :: Monad f => f a -> f b -> f b
fa >?= fb = fa >>= (const fb)

adapt :: Ord k => k -> (k, v) -> Map k v -> Map k v
adapt k (k', v) = M.insert k v . M.delete k

filterByKey :: (k -> Bool) -> Map k v -> Map k v
filterByKey p = M.filterWithKey (\k _ -> p k)

kingCastleRookS  = (8, 1)
queenCastleRookS = (1, 1)
kingCastleRookE  = (6, 1)
queenCastleRookE = (4, 1)
kingCastleKing   = (7, 1)
queenCastleKing  = (3, 1)

invert :: Colour -> Colour
invert B = W
invert W = B
invert T = T

colour :: Piece -> Colour
colour (Pawn _ c) = c
colour (Rook _ c) = c
colour (King c) = c
colour (Queen c) = c
colour (Bishop _ c) = c
colour (Knight _ c) = c
colour Empty = T

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
black = (== B) . colour

white :: Piece -> Bool
white = (== W) . colour

empty :: Piece -> Bool
empty Empty = True
empty _ = False

opposite :: Colour -> Piece -> Bool
opposite B = white
opposite W = black
opposite _ = (const False)

lookAt:: Pos -> Board -> Maybe Piece
lookAt p (Board ps _) = M.lookup p ps

lookFor :: Piece -> Board -> Maybe Pos
lookFor pc (Board _ pcs) = M.lookup pc pcs

attack :: Pos -> Colour -> Board -> Maybe Move
attack pos colour = fmap (const (Attack pos)) . mfilter viable . lookAt pos
        where viable piece = (opposite colour piece) || (empty piece)

take' :: Pos -> Colour -> Board -> Maybe Move
take' pos colour = fmap (const (Take pos)) . mfilter (opposite colour) . lookAt pos

block :: Pos -> Board -> Maybe Move
block pos = fmap (const (Block pos)) . mfilter empty . lookAt pos

jump :: Pos -> Board -> Maybe Move
jump pos = fmap (const (Jump pos)) . mfilter empty . lookAt pos

-- DO BETTER. MOVE FROM LIST TO SET
attackf :: (Pos -> Pos) -> Pos -> Colour -> Board -> [Move]
attackf f pos colour board = keepValid $ iterate f pos -- Do better, create a set directly 
    where keepValid (pos : ps) = let piece = lookAt pos board
                                 in keep piece ps 
          keep (Just piece) ps | empty piece = (Attack pos) : (keepValid ps) 
          keep (Just piece) ps | opposite colour piece = (Attack pos) : []
          keep _ _ = []

-- En Passant move missing
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
moves pos Empty             = const []

mposition :: Move -> Pos
mposition (Take p) = p
mposition (Block p) = p
mposition (Attack p) = p

threatsOf :: Colour -> Board -> Set Pos
threatsOf colour board = M.foldlWithKey gatherThreats S.empty $ filterByKey (opposite colour) $ pieces board
        where gatherThreats set piece pos = set <> (S.fromList $ fmap mposition $ filter threats $ moves pos piece board)
              threats (Attack _) = True
              threats (Take _) = True
              threats _ = False

check :: Pos -> Piece -> Move -> Board -> Maybe Move 
check _ piece m board = fmap (const m) $ mfilter (not . inCheck) $ M.lookup king $ pieces board
    where ocolour = invert $ colour piece
          king = King ocolour
          inCheck pos = S.member pos threats
          threats = threatsOf ocolour board

checkmate :: Pos -> Piece -> Move -> Board -> Maybe Move
checkmate pos piece m board = fmap (const m) $ mfilter (not . inCheckmate) $ M.lookup king $ pieces board
    where ocolour = invert $ colour piece
          king = King ocolour
          inCheckmate pos = (inCheck pos) && (cannotMove pos) && unblockable
          inCheck kingPos = S.member kingPos threats
          cannotMove kingPos = S.isSubsetOf (kingMoves kingPos) threats
          unblockable = isNothing $ check pos piece m $ apply pos m board 
          kingMoves kingPos = S.fromList $ fmap mposition $ moves kingPos king board
          threats = threatsOf ocolour board

checked :: Pos -> Piece -> Move -> Board -> Maybe Move
checked pos piece m board = check pos piece m $ apply pos m board 

-- Stalemate check missing
-- Chess total number of moves check missing
checks :: Pos -> Piece -> Move -> Board -> Maybe Move -- maybe outcome
checks pos piece move board = (check pos piece move board) <|> (checkmate pos piece move board)

qcastle :: Pos -> Piece -> Move -> Board -> Maybe Move
qcastle _ piece m board = fmap (const m) $ mfilter (const (not blocked)) $ (pure (&&) <*> hasKing <*> hasRook)
    where c = colour piece
          king p = p == (King c)
          rook p = (p == (Rook 1 c)) || (p == (Rook 2 c))
          hasKing = fmap (const True) $ mfilter king $ lookAt queenCastleKing board
          hasRook = fmap (const True) $ mfilter rook $ lookAt queenCastleRookS board
          blocked = S.isSubsetOf safetyPath threats
          safetyPath = S.fromList [(7, 1), (6, 1), (5, 1)]
          threats = threatsOf (invert c) board        

kcastle :: Pos -> Piece -> Move -> Board -> Maybe Move
kcastle _ piece m board = fmap (const m) $ mfilter (const (not blocked)) $ (pure (&&) <*> hasKing <*> hasRook)
            where c = colour piece
                  king p = p == (King c)
                  rook p = (p == (Rook 1 c)) || (p == (Rook 2 c))
                  hasKing = fmap (const True) $ mfilter king $ lookAt kingCastleKing board
                  hasRook = fmap (const True) $ mfilter rook $ lookAt kingCastleRookS board
                  blocked = S.isSubsetOf safetyPath threats
                  safetyPath = S.fromList [(7, 1), (6, 1), (5, 1)]
                  threats = threatsOf (invert c) board        

legality :: Pos -> Piece -> Move -> Board -> Maybe Move
legality pos p m @ (Take pos') board   = (checks pos p m board) >?= (checked pos p m board)
legality pos p m @ (Block pos') board  = (checks pos p m board) >?= (checked pos p m board)
legality pos p m @ (Attack pos') board = (checks pos p m board) >?= (checked pos p m board)
legality pos p m @ (Jump pos') board   = (checks pos p m board) >?= (checked pos p m board)
legality pos p m @ QCastle board       = (checks pos p m board) >?= (checked pos p m board) >?= (qcastle pos p m board)
legality pos p m @ KCastle board       = (checks pos p m board) >?= (checked pos p m board) >?= (kcastle pos p m board)

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
apply pos KCastle       = streamLine [(pos, kingCastleKing),  (kingCastleRookS, kingCastleRookE)]
apply pos QCastle       = streamLine [(pos, queenCastleKing), (queenCastleRookS, queenCastleRookE)]

move :: Pos -> Move -> Board -> Board
move pos m board = fromMaybe board newBoard
    where newBoard = do
            piece <- lookAt pos board 
            _     <- find (== m) $ moves pos piece board
            _     <- legality pos piece m board
            return (apply pos m board)

---- CHESS PIECES HAVE THEIR OWN UNICODE CHARACTERS!