module Chess.Internal where

import Data.Tuple (swap)
import Data.List (find, sortOn, groupBy)
import Control.Monad (mfilter, join)
import Data.Maybe (maybe, isJust, fromJust)
import Lib.Coll
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as M
import qualified Data.Set as S

-- Instead of translating a board on-demand, why not directly encode the pgn form of every move into the board?

type Coord       = (Int, Int)
data Colour      = B | W deriving (Eq, Ord)
data Piece       = Pawn | Knight | Bishop | Rook | Queen | King | Empty deriving (Eq, Show, Ord)
type Square      = (Colour, Coord)
type Figure      = (Piece, Colour)
data Position    = Pos Piece Colour Coord deriving (Eq, Ord)
type Coordinates = Map Coord Position
type Pieces      = Map Colour (Map Piece (Set Coord))

data Move = Capture   Position Position       |
            Advance   Position Coord          | 
            Enpassant Position Coord Position |
            Promote   Position Piece Position | 
            Castle   (Position, Coord) 
                     (Position, Coord)
            deriving (Eq, Ord) 

data Dir = U  | D  | L  | R |
           UL | UR | DL | DR deriving (Show, Eq, Ord)

data Castles = Short | Long | Both | None deriving (Eq, Ord)

data Board = Board { player      :: Colour,
                     check       :: Bool,
                     past        :: [Move],
                     coordinates :: Coordinates,
                     pieces      :: Pieces, 
                     blackCastle :: Castles,
                     whiteCastle :: Castles,
                     halfmoves   :: Int,
                     fullmoves   :: Int }
             deriving (Eq, Ord)

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

{- 
   A function that can follow any number of directions on the board and accepts a handler `h` that applies when it goes outside the board's bounds.
   By returning `[]`, `h` can be used to short-circuit the result
-}
followWith :: Board -> Square -> ([(Square, Position)] -> [(Square, Position)]) -> [Dir] -> [(Square, Position)]
followWith board s0 h dirs = fst $ foldr gather id dirs ([], s0)
      where gather dir f (xs, square) = case (lookAhead dir square) of
                        (Just position) -> (xs, square) `seq` f ((s0, position) : xs, push square position)
                        (Nothing)       -> (h xs, square)
            push (c, s) (Pos _ _ e)   = (c, e)
            lookAhead dir square      = lookAt board $ snd $ develop dir square 

-- Follows the board in that `Dir` until the board ends
follow :: Board -> Dir -> Square -> [(Square, Position)]
follow board dir s0 = followWith board s0 id $ repeat dir

-- Strictly follows the board for as many `[Dir]`s and short-circuits with `[]` if it can't
follow' :: Board -> [Dir] -> Square -> [(Square, Position)]
follow' board dirs s0 = followWith board s0 (const []) dirs 

other :: Colour -> Colour
other W = B 
other B = W

position :: Move -> Position
position (Advance p _)     = p
position (Capture p _)     = p
position (Promote p _ _)   = p
position (Enpassant p _ _) = p
position (Castle (p, _) _) = p 

figure :: Position -> Figure
figure (Pos piece colour _) = (piece, colour)

piece :: Position -> Piece
piece (Pos p _ _) = p

colour :: Position -> Colour
colour (Pos _ c _) = c

coord :: Position -> Coord
coord (Pos _ _ s) = s

square :: Position -> Square
square (Pos _ c s) = (c, s)

set :: Piece -> Square -> Position
set p (c, s) = Pos p c s

lookAt :: Board -> Coord -> Maybe Position
lookAt board coord = M.lookup coord $ coordinates board

empty :: (Square, Position) -> Bool
empty (_, (Pos p _ _)) = p == Empty

opposites :: (Square, Position) -> Bool
opposites ((c, _), (Pos _ c' _)) = c /= c'

opponent :: (Square, Position) -> Bool
opponent = every [not . empty, opposites]

jumped :: Board -> (Square, Position) -> Bool
jumped board ((colour, _), (Pos _ _ (x', y'))) = isJust $ mfilter opposed $ first $ past board 
      where opposed (Advance (Pos p c (xs, ys)) (xe, ye)) = (Pawn          == p) && -- it's a pawn 
                                                            (colour        /= c) && -- and my opponent
                                                            (abs (y' - ys) == 1) && -- is behind my new position
                                                            (x'           == xe)    -- we have the same file
            opposed _                                     = False


started :: Board -> (Square, Position) -> Bool
started board ((colour, coord), _)       = isJust $ mfilter (pawnOf colour) $ lookAt board coord
      where pawnOf W (Pos Pawn W (_, 2)) = True
            pawnOf B (Pos Pawn B (_, 7)) = True
            pawnOf _ _                   = False

safe :: Board -> (Castles -> (Square, Position) -> Bool)
safe board = let attackers = threats board
             in \castle s -> case (castle, fst $ fst s) of 
                                  (Short, W) -> null $ attackers [(W, (6, 1)), (W, (7, 1))]
                                  (Short, B) -> null $ attackers [(B, (6, 8)), (B, (7, 8))]
                                  (Long, W)  -> null $ attackers [(W, (3, 1)), (W, (4, 1))]
                                  (Long, B)  -> null $ attackers [(B, (3, 8)), (B, (4, 8))]
                                  (_, _)     -> False

backrank :: (Square, Position) -> Bool
backrank ((W, (_, 7)), (Pos _ _ (_, 8))) = True
backrank ((B, (_, 2)), (Pos _ _ (_, 1))) = True
backrank _                             = False

castleable :: Board -> Castles -> (Square, Position) -> Bool
castleable board = let attackers = threats board
                       wcastle   = whiteCastle board
                       bcastle   = blackCastle board
                  in \castle (s, p) -> case (castle, fst s) of
                         (Short, W) -> (wcastle == Both || wcastle == Short) && (null $ attackers [(W, (6, 1)), (W, (7, 1))])
                         (Short, B) -> (bcastle == Both || bcastle == Short) && (null $ attackers [(B, (6, 8)), (B, (7, 8))])
                         (Long,  W) -> (wcastle == Both || wcastle == Long)  && (null $ attackers [(W, (3, 1)), (W, (4, 1))])
                         (Long,  B) -> (bcastle == Both || bcastle == Long)  && (null $ attackers [(B, (3, 8)), (B, (4, 8))])
                         _          -> False

advance :: Piece -> (Square, Position) -> Move
advance piece ((c, s), (Pos _ _ e)) = Advance (Pos piece c s) e

capture :: Piece -> (Square, Position) -> Move
capture piece ((c, s), enemy) = Capture (Pos piece c s) enemy

enpassant :: Board -> (Square, Position) -> Move
enpassant board ((c, s), (Pos _ _ e)) = Enpassant (Pos Pawn c s) e enemy
      where enemy = fromJust $ lookAt board $ snd $ develop D (c, e)

promoteTo :: Piece -> (Square, Position) -> Move
promoteTo piece ((c, s), square) = Promote (Pos Pawn c s) piece square

castle :: (Move, Move) -> Move
castle (Advance k kp, Advance r rp) = Castle (k, kp) (r, rp)

pawnMoves :: Board -> Square -> [Move]
pawnMoves board = conjoin [ keepLast . consume [when (every [started board, empty]) $ advance Pawn]     . follow' board [U, U],
                            consume [when empty                                     $ advance Pawn]     . follow' board [U],
                            consume [once opponent                                  $ capture Pawn]     . follow' board [UR],
                            consume [once opponent                                  $ capture Pawn]     . follow' board [UL],
                            consume [once (every [jumped board, empty])             $ enpassant board]  . follow' board [UR],
                            consume [once (every [jumped board, empty])             $ enpassant board]  . follow' board [UL],
                            consume [once (every [backrank, empty])                 $ promoteTo Queen]  . follow' board [U],
                            consume [once (every [backrank, empty])                 $ promoteTo Knight] . follow' board [U],
                            consume [once (every [backrank, empty])                 $ promoteTo Rook]   . follow' board [U],
                            consume [once (every [backrank, empty])                 $ promoteTo Bishop] . follow' board [U],
                            consume [once (every [backrank, opponent])              $ promoteTo Queen]  . follow' board [UL],
                            consume [once (every [backrank, opponent])              $ promoteTo Knight] . follow' board [UL],
                            consume [once (every [backrank, opponent])              $ promoteTo Rook]   . follow' board [UL],
                            consume [once (every [backrank, opponent])              $ promoteTo Bishop] . follow' board [UL],
                            consume [once (every [backrank, opponent])              $ promoteTo Queen]  . follow' board [UR],
                            consume [once (every [backrank, opponent])              $ promoteTo Knight] . follow' board [UR],
                            consume [once (every [backrank, opponent])              $ promoteTo Rook]   . follow' board [UR],
                            consume [once (every [backrank, opponent])              $ promoteTo Bishop] . follow' board [UR]]

bishopMoves :: Board -> Square -> [Move]
bishopMoves board = conjoin [ consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board UL,
                              consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board UR,
                              consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board DR,
                              consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board DL]

rookMoves :: Board -> Square -> [Move]
rookMoves board = conjoin [ consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board U,
                            consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board D,
                            consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board L,
                            consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board R]

knightMoves :: Board -> Square -> [Move]
knightMoves board = conjoin [ consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [U, U, L],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [U, U, R],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [D, D, L],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [D, D, R],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [L, L, U],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [L, L, D],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [R, R, U],
                              consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [R, R, D]]

queenMoves :: Board -> Square -> [Move]
queenMoves board = conjoin  [ consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board U,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board D,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board L,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board R,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board UL,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board UR,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board DL,
                              consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board DR]

kingDevelopmentMoves :: Board -> Square -> [Move]
kingDevelopmentMoves board = conjoin [ consume [when empty $ advance King, once opponent $ capture King] . follow' board [U],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [D],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [L],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [R],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [UL],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [UR],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [DL],
                                       consume [when empty $ advance King, once opponent $ capture King] . follow' board [DR]]

kingCastlingMoves :: Board -> Square -> [Move]
kingCastlingMoves board = conjoin [map castle . zipped (keepLast . consume [exactly (every [empty, canCastle Short]) $ advance King] . follow' board [R, R])
                                                       (keepLast . consume [exactly empty                            $ advance Rook] . follow' board [L, L] . kingside),
                                   map castle . zipped (keepLast . consume [exactly (every [empty, canCastle Long])  $ advance King] . follow' board [L, L])
                                                       (keepLast . consume [exactly empty                            $ advance Rook] . follow' board [R, R, R] . queenside)]
      where canCastle        = castleable board
            kingside  (W, _) = (W, (8, 1))
            kingside  (B, _) = (B, (8, 8))
            queenside (W, _) = (W, (1, 1))
            queenside (B, _) = (B, (1, 8))

kingMoves :: Board -> Square -> [Move]
kingMoves board = conjoin [kingDevelopmentMoves board, kingCastlingMoves board] 

threats :: Board -> ([Square] -> [Move])
threats board sqs = filter attacks $ threateningMoves board
      where conflict (Pos _ c _) (Pos p _ e) = any (\(c', s) -> p /= Empty && c /= c' && e == s) sqs
            attacks (Capture pos enemy)      = conflict pos enemy
            attacks (Promote pos _ enemy)    = conflict pos enemy
            attacks (Enpassant pos _ enemy)  = conflict pos enemy 
            attacks _                        = False

threateningMoves :: Board -> [Move]
threateningMoves board = filter threat $ movesForThreats board (other $ player board)
      where threat (Capture _ _)      = True
            threat (Promote _ _ e)    = (colour e == player board) && (piece e /= Empty) 
            threat (Enpassant _ _ _)  = True
            threat  _                 = False

allMoves :: Board -> [Move]
allMoves board = join $ fmap (filter (isJust . permit board) .  movesPosition board) $ M.elems $ coordinates board

-- Internal use, omits castling because it causes infinite recursion. For calculating threats.  
movesForThreats :: Board -> Colour -> [Move]
movesForThreats board colour = M.foldrWithKey gatherMoves [] $ maybe M.empty id $ M.lookup colour $ pieces board
      where accumulateWith f coord moves    = foldr (:) moves $ f board (colour, coord) 
            gatherMoves King coords moves   = foldr (accumulateWith kingDevelopmentMoves) moves coords 
            gatherMoves Queen coords moves  = foldr (accumulateWith queenMoves) moves coords
            gatherMoves Bishop coords moves = foldr (accumulateWith bishopMoves) moves coords
            gatherMoves Rook coords moves   = foldr (accumulateWith rookMoves) moves coords
            gatherMoves Knight coords moves = foldr (accumulateWith knightMoves) moves coords
            gatherMoves Pawn coords moves   = foldr (accumulateWith pawnMoves) moves coords
            gatherMoves _    _ moves        = moves

movesAt :: Board -> Square -> [Move]
movesAt board = join . spread [pawnMoves board, kingMoves board, rookMoves board, bishopMoves board, knightMoves board, queenMoves board]

-- I should call `permit`
movesPosition :: Board -> Position -> [Move]
movesPosition board (Pos Pawn c s)   = pawnMoves board (c, s)
movesPosition board (Pos King c s)   = kingMoves board (c, s)
movesPosition board (Pos Rook c s)   = rookMoves board (c, s)
movesPosition board (Pos Bishop c s) = bishopMoves board (c, s)
movesPosition board (Pos Queen c s)  = queenMoves board (c, s)
movesPosition board (Pos Knight c s) = knightMoves board (c, s)
movesPosition board (Pos Empty _ _)  = []

movesPiece :: Board -> Figure -> [Move]
movesPiece board = filter (isJust . permit board) . join . map (movesPosition board) . findPieces board 

-- I should call `permit`
movesColour :: Board -> Colour -> [Move]
movesColour board colour = M.foldrWithKey gatherMoves [] $ maybe M.empty id $ M.lookup colour $ pieces board
      where accumulateWith f coord moves    = foldr (:) moves $ f board (colour, coord) 
            gatherMoves King coords moves   = foldr (accumulateWith kingMoves) moves coords 
            gatherMoves Queen coords moves  = foldr (accumulateWith queenMoves) moves coords
            gatherMoves Bishop coords moves = foldr (accumulateWith bishopMoves) moves coords
            gatherMoves Rook coords moves   = foldr (accumulateWith rookMoves) moves coords
            gatherMoves Knight coords moves = foldr (accumulateWith knightMoves) moves coords
            gatherMoves Pawn coords moves   = foldr (accumulateWith pawnMoves) moves coords
            gatherMoves _    _ moves        = moves

findPieces :: Board -> Figure -> [Position]
findPieces board (p, c) = maybe [] (asListOf (Pos p c)) $ M.lookup p $ maybe M.empty id $ M.lookup c $ pieces board

computeCastles :: Move -> Board -> Board
computeCastles move board = case move of
      (Castle (Pos _ W _, _) _)       -> board { whiteCastle = None }
      (Castle (Pos _ B _, _) _)       -> board { blackCastle = None }
      (Advance (Pos King W (5, 1)) _) -> board { whiteCastle = None }
      (Advance (Pos King B (5, 8)) _) -> board { blackCastle = None }
      (Advance (Pos Rook W (8, 1)) _) -> board { whiteCastle = longOrNone  $ whiteCastle board }
      (Advance (Pos Rook B (8, 8)) _) -> board { blackCastle = longOrNone  $ blackCastle board }
      (Capture (Pos Rook W (8, 1)) _) -> board { whiteCastle = longOrNone  $ whiteCastle board }
      (Capture (Pos Rook B (8, 8)) _) -> board { blackCastle = longOrNone  $ blackCastle board }
      (Advance (Pos Rook W (1, 1)) _) -> board { whiteCastle = shortOrNone $ whiteCastle board }
      (Advance (Pos Rook B (1, 8)) _) -> board { blackCastle = shortOrNone $ blackCastle board }
      (Capture (Pos Rook W (1, 1)) _) -> board { whiteCastle = shortOrNone $ whiteCastle board }
      (Capture (Pos Rook B (1, 8)) _) -> board { blackCastle = shortOrNone $ blackCastle board }
      _                               -> board
      where longOrNone Both    = Long
            longOrNone Short   = None
            longOrNone castle  = castle
            shortOrNone Both   = Short
            shortOrNone Long   = None
            shortOrNone castle = castle

-- If I keep `Empty`, it will be slower
reconstruct :: Board -> [Either Position Position] -> Board
reconstruct board updates = patch $ foldr rewrite (coordinates board, pieces board) updates
      where patch (coordinates, pieces) = board { coordinates = coordinates, pieces = pieces }
            detach (Pos Empty _ _) cs   = cs
            detach (Pos _ _ c) cs       = S.delete c cs
            attach (Pos _ _ c) cs       = S.insert c cs
            rewrite (Right pos @ (Pos p c r)) (coordinates, pieces) = (M.insert r pos coordinates, M.adjust (M.adjust (attach pos) p) c pieces)
            rewrite (Left  pos @ (Pos p c r)) (coordinates, pieces) = (M.insert r (Pos Empty W r) coordinates, M.adjust (M.adjust (detach pos) p) c pieces)

changePlacement :: Move -> Board -> Board
changePlacement move board = case move of
      (Capture (Pos p c s) (Pos p' c' e))          -> reconstruct board [Right (Pos p c e), Left (Pos p c s), Left (Pos p' c' e)]
      (Advance (Pos p c s) e)                      -> reconstruct board [Right (Pos p c e), Left (Pos p c s)]
      (Enpassant (Pos p c s) e (Pos p' c' s'))     -> reconstruct board [Right (Pos p c e), Left (Pos p c s), Left (Pos p' c' s')]
      (Promote (Pos p c s) p'' (Pos p' c' e))      -> reconstruct board [Right (Pos p'' c e), Left (Pos p c s), Left (Pos p' c' e)]
      (Castle (Pos k kc ks, ke) (Pos r rc rs, re)) -> reconstruct board [Right (Pos k kc ke), Right (Pos r rc re), Left (Pos k kc ks), Left (Pos r rc rs)]
 
changePlayers :: Board -> Board
changePlayers board = board { player = other $ player board }

trackMove :: Move -> Board ->  Board
trackMove move board = board { past = move : (past board) } 

computeChecks :: Board -> Board
computeChecks board = board { check = checked board }

-- total moves is incremented after black's move
countMoves :: Move -> Board -> Board
countMoves move board = board { halfmoves = half move, fullmoves = (fullmoves board) + increment }
      where half (Capture _ _)              = 0
            half (Advance (Pos Pawn _ _) _) = 0
            half (Enpassant _ _ _)          = 0
            half (Promote _ _ _)            = 0
            half _                          = (halfmoves board) + 1
            increment                       = case (past board) of (x:y:_) -> 1
                                                                   _       -> 0
            
permit :: Board -> Move -> Maybe Board
permit board move = let board' = forceApply board move
                        colour = player board
                        tboard = board' { player = colour } 
                    in case (check board) of True  | not $ checked tboard -> Just board' -- check and can escape
                                             False | not $ checked tboard -> Just board' -- not in check, but don't move into check
                                             _                            -> Nothing
apply :: Board -> Move -> Maybe Board
apply board move = join $ fmap (permit board) $ find (== move) $ movesPosition board $ position move

permitApply :: Board -> Move -> Maybe Board
permitApply board = permit board

-- `checked` is applied on the current board with the current player. Make sure to change the players to compute it propely 
forceApply :: Board -> Move -> Board
forceApply board move = computeChecks
                      $ changePlayers
                      $ countMoves move
                      $ computeCastles move 
                      $ trackMove move 
                      $ changePlacement move board

checked :: Board -> Bool
checked board = not $ null $ threats board [square king]
      where king = head $ findPieces board (King, player board)

immoble :: Board -> Bool
immoble board = all (checked . reset . forceApply board) $ movesForThreats board (player board)
      where reset b = b { player = player board }

checkmate :: Board -> Bool
checkmate = every [check, immoble]

-- as far as i remember, checking for stalemate is fairly simple. I think i can keep track of it's condition as a top-level field
stalemate :: Board -> Bool
stalemate = every [not . check, immoble]

drawn :: Board -> Bool
drawn board = False -- probably this

emptyBoard :: Board
emptyBoard = Board { player      = W,
                     past        = [],
                     check       = False,
                     blackCastle = Both,
                     whiteCastle = Both,
                     halfmoves   = 0,
                     fullmoves   = 0,
                     coordinates = M.fromList $ map (\p -> (coord p, p)) $ positions,
                     pieces      = M.fromList $ map byPiece $ groupOn colour $ filter (not . (== Empty) . piece) $ positions }
      where figs  = [([Rook, Knight, Bishop, Queen, King,  Bishop, Knight, Rook], W),
                     ([Pawn, Pawn,   Pawn,   Pawn,  Pawn,  Pawn,   Pawn,   Pawn], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Pawn, Pawn,   Pawn,   Pawn,  Pawn,  Pawn,   Pawn,   Pawn], B),
                     ([Rook, Knight, Bishop, Queen, King,  Bishop, Knight, Rook], B)]
            positions = do (y, (ps, c)) <- zip [1..] figs 
                           (x, p)       <- zip [1..] ps
                           return (Pos p c (x, y))
            set (p, es) = (p, S.fromList $ map coord es)
            byPiece (c, ps) = (c, M.fromList $ map set $ groupOn piece $ ps)