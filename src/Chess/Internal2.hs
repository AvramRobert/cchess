module Chess.Internal2 where

import Data.Tuple (swap)
import Data.List (find, sortOn)
import Data.List.NonEmpty (unfoldr, toList)
import Control.Monad (mfilter, join)
import Control.Applicative ((<|>))
import Data.Maybe (maybe, isJust, isNothing, catMaybes, fromJust)
import Lib.Coll
import Lib.Scalar

type Coord    = (Integer, Integer)
data Colour   = B | W deriving (Eq, Show, Ord)
data Piece    = Pawn | Knight | Bishop | Rook | Queen | King | Empty deriving (Eq, Show, Ord)
type Square   = (Colour, Coord)
data Position = Pos Piece Colour Coord deriving (Eq, Ord)

data Move = Capture   Position Coord       |
            Advance   Position Coord       | 
            Enpassant Position Coord Coord |
            Promote   Position Piece Coord |
            Castle   (Position, Coord) 
                     (Position, Coord)
            deriving (Eq, Show, Ord) 

data Dir = U  | D  | L  | R |
           UL | UR | DL | DR deriving (Show, Eq, Ord)

data Outcome = Continue       |
               Draw           |
               Stalemate      |
               Forfeit Colour | 
               Checkmate Colour deriving (Show, Eq, Ord)

data Board = Board { player          :: Colour,
                     check           :: Bool,
                     past            :: [Move],
                     pieces          :: [Position],
                     kingsideCastle  :: (Bool, Bool),
                     queensideCastle :: (Bool, Bool) } deriving (Eq)

instance Show Board where
      show board = unlines [whiteView $ pieces board, statistics board] -- make this based on the player? 

instance Show Position where
      show (Pos Pawn W xy)   = "♙ W" <> show xy
      show (Pos Pawn B xy)   = "♟ B" <> show xy
      show (Pos Rook W xy)   = "♖ W" <> show xy
      show (Pos Rook B xy)   = "♜ B" <> show xy
      show (Pos Bishop W xy) = "♗ W" <> show xy
      show (Pos Bishop B xy) = "♝ B" <> show xy
      show (Pos Knight W xy) = "♘ W" <> show xy
      show (Pos Knight B xy) = "♞ B" <> show xy 
      show (Pos King W xy)   = "♔ W" <> show xy
      show (Pos King B xy)   = "♚ B" <> show xy
      show (Pos Queen W xy)  = "♕ W" <> show xy
      show (Pos Queen B xy)  = "♛ B" <> show xy
      show (Pos Empty _ xy)  = "-  " <> show xy

makeRow :: Show a => [a] -> String
makeRow = foldl (\l c -> l <> (show c) <> " | ") "| "

chunksOf :: Int -> [a] -> [[a]]
chunksOf n = takeWhile (not . null) . map (take n) . iterate (drop n)

blackView :: [Position] -> String
blackView = unlines . map makeRow . chunksOf 8 . sortOn (swap . coord)

whiteView :: [Position] -> String
whiteView = unlines . map makeRow . reverse . chunksOf 8 . sortOn (swap . coord)

statistics :: Board -> String
statistics (Board player check past _ kc qc) = unlines ["Player:     " <> show player,
                                                        "In-Check:   " <> show check,
                                                        "Can castle: " <> (verify $ pickCastle player)]
      where pickCastle B = (snd kc, snd qc)
            pickCastle W = (fst kc, fst qc)
            verify (True, True)  = "Both sides"
            verify (True, False) = "King-Side"
            verify (False, True) = "Queen-Side"
            verify _             = "False" 

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

follow :: Board -> Dir -> Square -> [(Square, Position)]
follow board d s = case (lookAhead board d s) of
      (Just p)  -> (s, p) : (follow board d $ square p)
      (Nothing) -> []

follow' :: Board -> [Dir] -> Square -> [(Square, Position)]
follow' board dirs s0 = if (length dirs == length path) 
                        then path
                        else []
      where path            = gather dirs s0
            gather [] _     = []
            gather (d:ds) s = case (lookAhead board d s) of
                  (Just p)  -> (s0, p) : (gather ds $ square p)
                  (Nothing) -> []

other :: Colour -> Colour
other W = B
other B = W

position :: Move -> Position
position (Advance p _)     = p
position (Capture p _)     = p
position (Promote p _ _)   = p
position (Enpassant p _ _) = p
position (Castle (p, _) _) = p 

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
lookAt board coord' = find ((== coord') . coord) $ pieces board

empty :: (Square, Position) -> Bool
empty (_, (Pos p _ _)) = p == Empty

opponent :: (Square, Position) -> Bool
opponent ((c, _), (Pos _ c' _)) = c /= c'

jumped :: Board -> (Square, Position) -> Bool
jumped board ((colour, (x, y)), _)  = isJust $ mfilter opposed $ keepFirst $ past board 
      where keepFirst ((Advance p ps) : _)                 = Just (Advance p ps)
            keepFirst []                                   = Nothing
            opposed (Advance (Pos p c (xs, ys)) (xe, ye))  = (Pawn == p) && 
                                                             (colour /= c) &&   
                                                             (abs (xs - x) == 1) && 
                                                             (abs (ye - ys) == 2)

started :: Board -> (Square, Position) -> Bool
started board ((colour, coord), _)       = isJust $ mfilter (pawnOf colour) $ lookAt board coord
      where pawnOf W (Pos Pawn W (_, 2)) = True
            pawnOf B (Pos Pawn B (_, 7)) = True
            pawnOf _ _                   = False

pawnOf :: Colour -> Position -> Bool
pawnOf W (Pos Pawn W (_, 2)) = True
pawnOf B (Pos Pawn B (_, 7)) = True
pawnOf _ _                   = False

safe :: Board -> (Dir -> (Square, Position) -> Bool)
safe board = let attackers = threats board
             in \dir s -> case (dir, fst $ fst s) of 
                               (R, W) -> null $ attackers [(W, (6, 1)), (W, (7, 1))]
                               (R, B) -> null $ attackers [(B, (6, 8)), (B, (7, 8))]
                               (L, W) -> null $ attackers [(W, (3, 1)), (W, (4, 1))]
                               (L, B) -> null $ attackers [(B, (3, 8)), (B, (4, 8))]
                               (_, _) -> False

backrank :: (Square, Position) -> Bool
backrank ((W, (_, 7)), (Pos _ _ (_, 8))) = True
backrank ((B, (_, 2)), (Pos _ _ (_, 1))) = True
backrank _                             = False

canCastle :: Board -> Dir -> (Square, Position) -> Bool
canCastle board L ((W, _), _) = fst $ queensideCastle board
canCastle board L ((B, _), _) = snd $ queensideCastle board
canCastle board R ((W, _), _) = fst $ kingsideCastle board
canCastle board R ((B, _), _) = snd $ kingsideCastle board
canCastle board _ _           = False

threats :: Board -> ([Square] -> [Move])
threats board = let moves = pieces board >>= (movesFor board)
                in \sqs -> filter (oneOf (fmap attacks sqs)) moves 
      where attacks (c, s) (Capture   (Pos _ c' _) e)    = c /= c' && s == e
            attacks (c, s) (Enpassant (Pos _ c' _) _ e)  = c /= c' && s == e
            attacks (c, s) (Promote   (Pos _ c' _) _ e)  = c /= c' && s == e
            attacks _ _                                  = False

advance :: Piece -> (Square, Position) -> Move
advance piece ((c, s), (Pos _ _ e)) = Advance (Pos piece c s) e

capture :: Piece -> (Square, Position) -> Move
capture piece ((c, s), (Pos _ _ e)) = Capture (Pos piece c s) e

enpassant :: (Square, Position) -> Move
enpassant ((c, s), (Pos _ _ e)) = Enpassant (Pos Pawn c s) e (fst s, (snd s) - 1)

promoteTo :: Piece -> (Square, Position) -> Move
promoteTo piece ((c, s), (Pos _ _ e)) = Promote (Pos Pawn c s) piece e

castle :: (Move, Move) -> Move
castle (Advance k kp, Advance r rp) = Castle (k, kp) (r, rp)

permitted :: Board -> Move -> Bool
permitted board move = (not $ check board) || (escapes $ perform board move)
      where escapes (Checkmate _, _) = True
            escapes (Stalemate, _)   = True
            escapes (_, board')      = not $ check board' 

compile :: Board -> [Square -> [Move]] -> Square -> [Move]
compile board moves = filter (permitted board) . conjoin moves

pawnMoves :: Board -> Square -> [Move]
pawnMoves board = compile board [   keepLast . consume [when empty              $ advance Pawn]     . follow' board [U, U],
                                    consume [when empty                         $ advance Pawn]     . follow' board [U],
                                    consume [once opponent                      $ capture Pawn]     . follow' board [UR],
                                    consume [once opponent                      $ capture Pawn]     . follow' board [UL],
                                    consume [once (every [jumped board, empty]) $ enpassant]        . follow' board [UR],
                                    consume [once (every [jumped board, empty]) $ enpassant]        . follow' board [UL],
                                    consume [once (every [backrank, empty])     $ promoteTo Queen]  . follow' board [U],
                                    consume [once (every [backrank, empty])     $ promoteTo Knight] . follow' board [U],
                                    consume [once (every [backrank, empty])     $ promoteTo Rook]   . follow' board [U],
                                    consume [once (every [backrank, empty])     $ promoteTo Bishop] . follow' board [U],
                                    consume [once (every [backrank, opponent])  $ promoteTo Queen]  . follow' board [UL],
                                    consume [once (every [backrank, opponent])  $ promoteTo Knight] . follow' board [UL],
                                    consume [once (every [backrank, opponent])  $ promoteTo Rook]   . follow' board [UL],
                                    consume [once (every [backrank, opponent])  $ promoteTo Bishop] . follow' board [UL],
                                    consume [once (every [backrank, opponent])  $ promoteTo Queen]  . follow' board [UR],
                                    consume [once (every [backrank, opponent])  $ promoteTo Knight] . follow' board [UR],
                                    consume [once (every [backrank, opponent])  $ promoteTo Rook]   . follow' board [UR],
                                    consume [once (every [backrank, opponent])  $ promoteTo Bishop] . follow' board [UR]]

bishopMoves :: Board -> Square -> [Move]
bishopMoves board = compile board [ consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board UL,
                                    consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board UR,
                                    consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board DR,
                                    consume [when empty $ advance Bishop, once opponent $ capture Bishop] . follow board DL]

rookMoves :: Board -> Square -> [Move]
rookMoves board = compile board [   consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board U,
                                    consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board D,
                                    consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board L,
                                    consume [when empty $ advance Rook, once opponent $ capture Rook] . follow board R]

knightMoves :: Board -> Square -> [Move]
knightMoves board = compile board [ consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [U, U, L],
                                    consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [U, U, R],
                                    consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [D, D, L],
                                    consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [D, D, R],
                                    consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [L, L, U],
                                    consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [L, L, D],
                                    consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [R, R, U],
                                    consume [when empty $ advance Knight, once opponent $ capture Knight] . keepLast . follow' board [R, R, D]]

queenMoves :: Board -> Square -> [Move]
queenMoves board = compile board [  consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board U,
                                    consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board D,
                                    consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board L,
                                    consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board R,
                                    consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board UL,
                                    consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board UR,
                                    consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board DL,
                                    consume [when empty $ advance Queen, once opponent $ capture Queen] . follow board DR]


kingMoves :: Board -> Square -> [Move]
kingMoves board = compile board [   consume [when empty $ advance King, once opponent $ capture King] . follow' board [U],
                                    consume [when empty $ advance King, once opponent $ capture King] . follow' board [D],
                                    consume [when empty $ advance King, once opponent $ capture King] . follow' board [L],
                                    consume [when empty $ advance King, once opponent $ capture King] . follow' board [R],
                                    consume [when empty $ advance King, once opponent $ capture King] . follow' board [UL],
                                    consume [when empty $ advance King, once opponent $ capture King] . follow' board [UR],
                                    consume [when empty $ advance King, once opponent $ capture King] . follow' board [DL],
                                    consume [when empty $ advance King, once opponent $ capture King] . follow' board [DR],
                                    map castle . zipped (keepLast . consume [when (every [empty, safeKingside, canCastle board R])  $ advance King] . follow' board [R, R])
                                                      (keepLast . consume [when (every [empty, canCastle board R])                $ advance Rook] . follow' board [L, L] . kingside),
                                    map castle . zipped (keepLast . consume [when (every [empty, safeQueenside, canCastle board L]) $ advance King] . follow' board [L, L])
                                                      (keepLast . consume [when (every [empty, canCastle board L])                $ advance Rook] . follow' board [R, R, R] . queenside)]
        where safecheck        = safe board
              safeKingside     = safecheck R
              safeQueenside    = safecheck L
              kingside  (W, _) = (W, (8, 1))
              kingside  (B, _) = (B, (8, 8))
              queenside (W, _) = (W, (1, 1))
              queenside (B, _) = (B, (1, 8))

moves :: Board -> [Move]
moves board = pieces board >>= (movesFor board)

movesAt :: Board -> Square -> [Move]
movesAt board = join . spread [pawnMoves board, kingMoves board, rookMoves board, bishopMoves board, knightMoves board, queenMoves board]

movesFor :: Board -> Position -> [Move]
movesFor board (Pos Pawn c s)   = pawnMoves board (c, s)
movesFor board (Pos King c s)   = kingMoves board (c, s)
movesFor board (Pos Rook c s)   = rookMoves board (c, s)
movesFor board (Pos Bishop c s) = bishopMoves board (c, s)
movesFor board (Pos Queen c s)  = queenMoves board (c, s)
movesFor board (Pos Knight c s) = knightMoves board (c, s)

movesOpponent :: Board -> [Move]
movesOpponent board = (filter ((== (other $ player board)) . colour) $ pieces board) >>= (movesFor board)

-- reconstruct by telling it which position to add and which coordinates to remove
reconstruct :: [Position] -> [Coord] -> [Position] -> [Position]
reconstruct ((Pos p c s) : ps) cs ((Pos _ _ r) : rs) | s == r = (Pos p c r) : (reconstruct ps cs rs)
reconstruct ps (c : cs) ((Pos _ _ r) : rs)           | c == r = (Pos Empty W r) : (reconstruct ps cs rs)
reconstruct [] [] rs                                          = rs
reconstruct ps cs (r : rs)                                    = r : (reconstruct ps cs rs)
reconstruct ps cs []                                          = []

castled :: Dir -> (Bool, Bool) -> Move -> (Bool, Bool)
castled R (w, b) (Castle (_, d) _)          = (w `xor` (d == (7, 1)), b `xor` (d == (7, 8)))
castled L (w, b) (Castle (_, d) _)          = (w `xor` (d == (3, 1)), b `xor` (d == (3, 8)))
castled R (w, b) (Advance (Pos Rook _ s) _) = (w && (s == (8, 1)), b && (s == (8, 8)))
castled L (w, b) (Capture (Pos Rook _ s) _) = (w && (s == (1, 1)), b && (s == (1, 8)))
castled _ (w, b) (Advance (Pos King _ s) _) = (w && (s == (5, 1)), b && (s == (5, 8)))
castled _ (w, b) (Capture (Pos King _ s) _) = (w && (s == (5, 1)), b && (s == (5, 8)))
castled _ (w, b) _                          = (w, b)

-- you have to compute the other states aswell
evaluate :: Board -> (Outcome, Board)
evaluate board = if      mate  then (Checkmate colour', board)
                 else if stale then (Stalemate, board) 
                 else               (Continue, board)
      where mate     = check board && immoble
            stale    = False -- check this
            immoble  = all (check . apply board) $ movesOpponent board
            king     = fromJust $ find (every [(== colour') . colour, (== King) . piece]) $ pieces board
            colour'  = player board
            
apply :: Board -> Move -> Board
apply board move = let  king   = fromJust $ find (every [(== player board') . colour, (== King) . piece]) $ pieces board'
                        board' = board { pieces          = commit move $ pieces board, 
                                         past            = move : (past board),
                                         kingsideCastle  = castled R (kingsideCastle board) move,
                                         queensideCastle = castled L (queensideCastle board) move,  
                                         player          = other $ player board }
                   in board' { check = not $ null $ threats board' [square king] }
      where commit (Capture (Pos p c s) e)      = reconstruct [(Pos p c e)] [s]
            commit (Advance (Pos p c s) e)      = reconstruct [(Pos p c e)] [s]
            commit (Enpassant (Pos p c s) e r)  = reconstruct [(Pos p c e)] [s, r]
            commit (Promote (Pos p c s) p' e)   = reconstruct [(Pos p' c e)] [s] 
            commit (Castle ((Pos k kc ks), ke) 
                           ((Pos r rc rs), re)) = reconstruct [(Pos k kc ke), (Pos r rc re)] [ks, rs]

perform :: Board -> Move -> (Outcome, Board) 
perform board = evaluate . apply board

board :: Board
board = Board { player          = W,
                past            = [],
                check           = False,
                kingsideCastle  = (True, True),
                queensideCastle = (True, True),
                pieces          = do (y, (ps, c)) <- zip [1..] figs 
                                     (x, p)       <- zip [1..] ps
                                     return (Pos p c (x, y)) }
      where figs  = [([Rook, Knight, Bishop, Queen, King,  Bishop, Knight, Rook], W),
                     ([Pawn, Pawn,   Pawn,   Pawn,  Pawn,  Pawn,   Pawn,   Pawn], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Empty, Empty, Empty,  Empty, Empty, Empty,  Empty, Empty], W),
                     ([Pawn, Pawn,   Pawn,   Pawn,  Pawn,  Pawn,   Pawn,   Pawn], B),
                     ([Rook, Knight, Bishop, Queen, King,  Bishop, Knight, Rook], B)]